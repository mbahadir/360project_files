# install the required packages first
require(jsonlite)
require(httr)
require(data.table)

get_token <- function(username, password, url_site){
    
    post_body = list(username=username,password=password)
    post_url_string = paste0(url_site,'/token/')
    result = POST(post_url_string, body = post_body)

    # error handling (wrong credentials)
    if(result$status_code==400){
        print('Check your credentials')
        return(0)
    }
    else if (result$status_code==201){
        output = content(result)
        token = output$key
    }

    return(token)
}

get_data <- function(start_date='2020-03-20', token, url_site){
    
    post_body = list(start_date=start_date,username=username,password=password)
    post_url_string = paste0(url_site,'/dataset/')
    
    header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
    result = GET(post_url_string, header, body = post_body)
    output = content(result)
    data = data.table::rbindlist(output)
    data[,event_date:=as.Date(event_date)]
    data = data[order(event_date)]
    return(data)
}


send_submission <- function(predictions, token, url_site, submit_now=F){
    
    format_check=check_format(predictions)
    if(!format_check){
        return(FALSE)
    }
    
    post_string="list("
    for(i in 1:nrow(predictions)){
        if(i<nrow(predictions)){
            post_string=sprintf("%s%s,",post_string,predictions$forecast[i])
        } else {
            post_string=sprintf("%s%s)",post_string,predictions$forecast[i])
        }
    }
    
    submission = eval(parse(text=post_string))
    json_body = jsonlite::toJSON(submission, auto_unbox = TRUE)
    submission=list(submission=json_body)
    
    print(submission)
    # {"31515569":2.4,"32939029":2.4,"4066298":2.4,"6676673":2.4,"7061886":2.4,"85004":2.4} 

    if(!submit_now){
        print("You did not submit.")
        return(FALSE)      
    }
    

    header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
    post_url_string = paste0(url_site,'/submission/')
    result = POST(post_url_string, header, body=submission)
    
    if (result$status_code==201){
        print("Successfully submitted. Below you can see the details of your submission")
    } else {
        print("Could not submit. Please check the error message below, contact the assistant if needed.")
    }
    
    print(content(result))
    
}

check_format <- function(predictions){
    
    if(is.data.frame(predictions) | is.data.frame(predictions)){
        if('forecast' %in% names(predictions)){
            if(nrow(predictions)==24){
                if(all(is.numeric(predictions$forecast))){
                    print("Format OK")
                    return(TRUE)
                } else {
                    print("forecast information is not numeric")
                    return(FALSE)                
                }
            } else {
                print("Forecasts for 24 hours should be provided, current number of rows:")
                print(nrow(predictions))
                return(FALSE)     
            }
        } 
    } else {
        print("Wrong format. Please provide data.frame or data.table object")
        return(FALSE)
    }
    
}

# this part is main code
subm_url = 'http://46.101.124.77'

u_name = "Group10"
p_word = "sSQe4kg1ne5XiB7U"
submit_now = FALSE

username = u_name
password = p_word

token = get_token(username=u_name, password=p_word, url=subm_url)
data = get_data(token=token,url=subm_url)


library(data.table)
library(dplyr)

data=fread("current_data.csv")

data=data[,c(2:11)]
data$Date=as.Date(data$Date)

# old <- c("event_date", "event_hour", "consumption","t_1","t_2","t_3","t_4","t_5","t_6","t_7")
# new <- c("Date", "Hour", "Consumption","T_1","T_2","T_3","T_4","T_5","T_6","T_7")
# setnames(data, old, new, skip_absent = TRUE)

data=data[order(Date,Hour)]

train=fread("bulk_consumption_with_temp.csv")
train$Date=as.Date(train$Date)

all_dt=rbind(train,data)

perf_dt=function(type,actual,forecast){
    name=type
    n=length(actual)
    error=actual-forecast
    mean=mean(actual)
    sd=sd(actual)
    FBias=sum(error)/sum(actual)
    MPE=sum(error/actual)/n
    MAPE=sum(abs(error/actual))/n
    RMSE=sqrt(sum(error^2))/n
    MAD=sum(abs(error))/n
    WMAPE=MAD/mean
    l=data.frame(name,n,mean,sd,FBias,MAPE,RMSE,MAD,WMAPE)
    return(l)
}

setnames(all_dt,"Date","date")
setnames(all_dt,"Consumption","consumption")
setnames(all_dt,"Hour","hour")

df =all_dt %>% 
  group_by(date) %>% 
  summarise(consumption = mean(consumption))
dt=as.data.table(df)

train=filter(dt,date<="2021-01-07")
test=filter(dt,date>"2021-01-07")

train_ts=ts(train$consumption,freq=7)
test_ts=ts(test$consumption,freq=7)

feat_dt=fread("C:/Users/bahad/GitHub/360project/all_features_df.csv")
feat_dt$date=as.Date(feat_dt$date)

feat_dt[holiday_val_factor==8,holiday_val_factor:=0]

train_feat=merge(train,feat_dt[,c(1,2)],by.x="date",by.y="date")
#train_feat[out_and$index][holiday_val_factor==0]$holiday_val_factor=8

train_feat$month=month(train_feat$date)

train_feat[,year_day:=yday(date)]
train_feat[,week_day:=wday(date)]
train_feat[,lag_7:=shift(consumption, 7)]
train_feat[,lag_14:=shift(consumption, 14)]
train_feat[,for_14:=shift(consumption, type="lead", 14)]
train_feat[,for_7:=shift(consumption, type="lead", 7)]
train_feat[, lag_365:=shift(consumption, 364)]

train_feat[holiday_val_factor==1,consumption:=lag_7]
train_feat[1]$consumption=train_feat[8]$consumption

train_feat[year_day==366]$consumption=train_feat[year_day==366]$lag_7

train_feat[year_day==365]$consumption=train_feat[year_day==365]$lag_7

train_feat[date=="2021-01-02", consumption:=lag_7]

train_feat[date<"2017-07-02" & date>="2017-06-23" & holiday_val_factor==0,holiday_val_factor:=8]

train_feat[date=="2017-08-29", holiday_val_factor:=8] 

train_feat[date=="2019-08-31", holiday_val_factor:=8] 

train_feat[date<"2018-08-27" & date>="2018-08-18" & holiday_val_factor==0,holiday_val_factor:=8]

train_feat[date<"2019-06-10" & date>="2019-06-01" & holiday_val_factor==0,holiday_val_factor:=8]

train_feat[date<"2019-08-19" & date>="2019-08-10" & holiday_val_factor==0,holiday_val_factor:=8]

train_feat[date=="2019-12-29",holiday_val_factor:=8]

train_feat[date<="2020-03-20" & date>="2020-03-16" & holiday_val_factor==0,holiday_val_factor:=8]

# train_feat[date>="2020-07-10" & date<="2020-08-15",]

train_feat[date>="2020-04-01"&date<="2020-05-31"& (week_day==1|week_day==7) & holiday_val_factor==0,holiday_val_factor:=9]

for(i in 2:8){
    for(j in 1:nrow(train_feat[holiday_val_factor==i,])){
        new_val=0
        count=0
        if(train_feat[date==train_feat[holiday_val_factor==i,][j]$date+7]$holiday_val_factor==0){
            new_val=new_val+train_feat[holiday_val_factor==i,][j]$for_7+train_feat[holiday_val_factor==i,][j]$for_7
            count=count+2
        }
        if(train_feat[date==train_feat[holiday_val_factor==i,][j]$date-7]$holiday_val_factor==0){
            new_val=new_val+train_feat[holiday_val_factor==i,][j]$lag_7+train_feat[holiday_val_factor==i,][j]$lag_7
            count=count+2
        }
        if(train_feat[date==train_feat[holiday_val_factor==i,][j]$date+14]$holiday_val_factor==0){
            new_val=new_val+train_feat[holiday_val_factor==i,][j]$lag_14
            count=count+1
        }
        new_val=new_val/count
        train_feat[holiday_val_factor==i,][j]$consumption=new_val        
    }
}

train_feat[holiday_val_factor==9 & week_day==7, consumption:=consumption+1500]

train_feat[date=="2020-05-16"|date=="2020-05-17",consumption:=lag_7]

# diff_sun=train_feat[date>="2019-04-01"&date<="2019-06-05" & (week_day==1), ]$consumption-train_feat[date>="2020-04-01"&date<="2020-05-31" & (week_day==1), ]$consumption
# diff_sat=train_feat[date>="2019-04-01"&date<="2019-06-05" & (week_day==7), ]$consumption-train_feat[date>="2020-04-01"&date<="2020-05-31" & (week_day==7), ]$consumption

# diff_sun
# summary(diff_sun)

# diff_sat
# summary(diff_sat)

# diff=train_feat[date>="2019-04-01"&date<="2019-05-31" & (week_day!=1 & week_day!=7), ]$consumption-train_feat[date>="2020-04-01"&date<="2020-06-02" & (week_day!=1 & week_day!=7), ]$consumption

# summary(diff)

# diff

last_decomp=decompose(ts(train_feat$consumption,freq=7),type='add')
plot(last_decomp)

library(forecast)

out_new<-tsoutliers(last_decomp$random)

# sub_decomp_new=decompose(ts(train_feat$consumption,freq=7),type='add')
# plot(sub_decomp_new)

# plot(train_feat[date>="2020-01-01"&date<="2021-01-30"& (week_day==7),]$date,
#      train_feat[date>="2020-01-01"&date<="2021-01-30"& (week_day==7),]$consumption,type="l")

# plot(train_feat[date>="2020-01-01"&date<="2021-01-30"& (week_day==1),]$date,
#      train_feat[date>="2020-01-01"&date<="2021-01-30"& (week_day==1),]$consumption,type="l")

# train_feat[date<="2021-01-30"& month== 4 & (week_day==7),mean(consumption),by=year(date)]

# train_feat[date<="2021-01-30"& month== 5 & (week_day==7),mean(consumption),by=year(date)]

# plot(train_feat[date<="2021-01-30"& (week_day==7) & holiday_val_factor==0,]$date,
#      train_feat[date<="2021-01-30"& (week_day==7)& holiday_val_factor==0,]$consumption,type="l")

# plot(train_feat[date<="2021-01-30"& (week_day==7) & holiday_val_factor==0,]$date,
#      train_feat[date<="2021-01-30"& (week_day==7)& holiday_val_factor==0,]$consumption,type="l")

library(urca)

unt_test_final=ur.kpss(last_decomp$random) 
summary(unt_test_final)

pred_dt=rbind(train_feat,test, fill=TRUE) 
pred_dt=pred_dt[,c(1,2)]
pred_dt=as.data.table(pred_dt)

fitted=auto.arima(last_decomp$random,trace=T)

fitted

tsdisplay(fitted$residuals, main="Residuals of the Arima Model")

fit_mod=arima(last_decomp$random, order=c(3,0,2), seasonal = list(order = c(0, 0, 1), period = 7))
fit_mod

tsdisplay(fit_mod$residuals, main="Residuals of the New Arima Model")

pred_dt=rbind(train_feat,test,fill=TRUE)
pred_dt=pred_dt[,c(1,2)]
pred_dt=as.data.table(pred_dt)

str(pred_dt)

# write.csv(data,"current_data.csv")

test_start=as.Date("2021-01-08")

results=vector("list",14)

for(i in 1:14){
    current_test_date=test_start+i
    train_data=pred_dt[date<current_test_date]
    test_data=pred_dt[date==current_test_date]
    print(current_test_date)
    decomp=decompose(ts(train_data$consumption,freq=7),type="add")
    rand_val=decomp$random
    fit_mod=arima(rand_val, order=c(3,0,2), seasonal = list(order = c(0, 0, 1), period = 7))
    test_data[,forecasted:=as.numeric(predict(fit_mod, n.ahead = 1)$pred + decomp$season[wday(current_test_date)] +
                                     tail(decomp$trend[!is.na(decomp$trend)],1))]
    results[[i+1]]=test_data
}

res_dt=rbindlist(results)

res_dt[, diff:=consumption-forecasted]

res_dt

perf_dt("Predictions in Arima with Slicing Window and without regressor", res_dt$consumption, res_dt$forecasted)

feature_cont=read.csv("C:/Users/bahad/GitHub/360project/all_features_df.csv")
feature_cont=as.data.table(feature_cont)
feature_cont$date=as.Date(feature_cont$date)
temp_all=fread("bulk_consumption_with_temp.csv")
temp_all$Date=as.Date(temp_all$Date)
temp_all=rbind(temp_all, data)

temp_df =temp_all %>% 
  group_by(Date) %>% 
  summarise(consumption = mean(Consumption),
            reduced_tmp = mean(0.3794502 * T_1 + 
                          0.3748089 * T_2 + 0.3782977 * T_3 + 
                          0.3800144 * T_4 + 0.3790882 * T_5 + 
                          0.3803995 * T_6 + 0.3736363 * T_7),
            reduced_tmp_max = max(0.3794502 * T_1 + 
                          0.3748089 * T_2 + 0.3782977 * T_3 + 
                          0.3800144 * T_4 + 0.3790882 * T_5 + 
                          0.3803995 * T_6 + 0.3736363 * T_7),
            reduced_tmp_min = min(0.3794502 * T_1 + 
                          0.3748089 * T_2 + 0.3782977 * T_3 + 
                          0.3800144 * T_4 + 0.3790882 * T_5 + 
                          0.3803995 * T_6 + 0.3736363 * T_7)
           )
temp_dt=as.data.table(temp_df)


final_feat_dt=merge(feature_cont[,c(1,4,5,6,7,8,9)],temp_dt[, c(1,3,4,5)],by.x="date", by.y="Date")

final_feat_dt[,abs_reduced_tmp:=abs(reduced_tmp-40)]

final_feat_dt[,abs_reduced_tmp_max:=abs(reduced_tmp_max-55)]

final_feat_dt[lock_down_factor==1,lock_down_factor:=0]
final_feat_dt[lock_down_factor==2,lock_down_factor:=1]

elim_feat=final_feat_dt[, c("date", "lock_down_factor", "covid_severity", "sunlight_time_minutes", 
                           "production_capacity_rate", "reduced_tmp", "reduced_tmp_min",
                           'abs_reduced_tmp', "abs_reduced_tmp_max")]

test_start=as.Date("2021-01-08")

results=vector("list",14)

for(i in 1:14){
    current_test_date=test_start+i
    train_data=pred_dt[date<current_test_date]
    xreg_train=unname(as.matrix(elim_feat[date<current_test_date,c(2,3,4,5,6,7,8,9)]))
    test_data=pred_dt[date==current_test_date]
    xreg_test=unname(as.matrix(elim_feat[date==current_test_date,c(2,3,4,5,6,7,8,9)]))
    decomp=decompose(ts(train_data$consumption,freq=7),type="add")
    rand_val=decomp$random  
    fit_mod=arima(rand_val, order=c(0,0,2), seasonal = list(order = c(1, 0, 0), period = 7), xreg=xreg_train)
    test_data[,forecasted:=as.numeric(predict(fit_mod, n.ahead = 1,newxreg=xreg_test)$pred + decomp$season[wday(current_test_date)] +
                                     tail(decomp$trend[!is.na(decomp$trend)],1))]
    print(current_test_date)
    pred_dt[date==current_test_date,consumption:=as.numeric(predict(fit_mod, n.ahead = 1,newxreg=xreg_test)$pred + decomp$season[wday(current_test_date)] +
                                     tail(decomp$trend[!is.na(decomp$trend)],1))]
    results[[i+1]]=test_data
}

res_dt=rbindlist(results)

res_dt[, diff:=consumption-forecasted]

res_dt

perf_dt("Predictions in Arima with Slicing Window and regressor", res_dt$consumption, res_dt$forecasted)

feature_dt_hour=fread("C:/Users/bahad/GitHub/360project/hourly_feature_df.csv")
feature_dt_hour=feature_dt_hour[,-1]
feature_dt_hour$date=as.Date(feature_dt_hour$date)

req_feature_hour=feature_dt_hour[date>"2020-06-06"&date<=Sys.Date()-7]

str(feature_dt_hour)

time_cons1=read.csv("bulk_consumption_with_temp.csv")
time_cons2=read.csv("current_data.csv")
time_cons2=time_cons2[,-1]

time_cons=rbind(time_cons1,time_cons2)
setnames(time_cons,"Date","date")
time_cons=as.data.table(time_cons)
time_cons$date=as.Date(time_cons$date)

str(time_cons)

time_cons[,cons_daily:= sum(Consumption) ,by=date]
time_cons[,perc:= Consumption/cons_daily]

for(i in 0:23) {
  assign(paste0("hour",i), time_cons[Hour==i & date>"2020-06-06"&date<=Sys.Date()-7,])
  }

for(i in 0:23){
    assign(paste0("decompose",i),decompose(ts(get(paste0("hour",i))$perc,freq=7),
                                           type='add'))
}

req_feature_hour$hour=rep(0:23,nrow(req_feature_hour)/24)

library(forecast)

res_hour_per=data.table(hour=0:23)

results_percentage=vector("list",24)

for(i in 0:23){
    new_forescasted=
    fit_mod=auto.arima(get(paste0("decompose",i))$random)
    new_forescasted=as.numeric(predict(fit_mod, n.ahead = 1)$pred + get(paste0("decompose",i))$season[wday(Sys.Date())] +
                                     tail(get(paste0("decompose",i))$trend[!is.na(get(paste0("decompose",i))$trend)],1))
    results_percentage[[i+1]]=new_forescasted
}

Percentage_dt=t(rbind(results_percentage))

last_res_daily=(res_dt[nrow(res_dt)]$forecasted)*24

last_res_hourly=last_res_daily*as.numeric(Percentage_dt)

last_res_hourly



alt_prc=read.csv("January_Consumption_Percentage.csv")
alt_prc=as.data.table(alt_prc)

last_res_hourly_alt=(alt_prc[week_day ==3, ]$percentage*last_res_daily)

perf_dt("hourly_perf",all_dt[date=="2021-01-26",]$consumption,last_res_hourly)

perf_dt("hourly_perf",all_dt[date=="2021-01-26",]$consumption,last_res_hourly_alt)

str(alt_prc)

alt_prc[week_day ==3, ]





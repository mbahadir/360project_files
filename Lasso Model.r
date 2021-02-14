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
library(tidyr)
library(MLmetrics)
library(glmnet)

dt=fread("bulk_consumption_with_temp.csv")
dt$Date=as.Date(dt$Date)

old <- c("event_date", "event_hour", "consumption","t_1","t_2","t_3","t_4","t_5","t_6","t_7")
new <- c("Date", "Hour", "Consumption","T_1","T_2","T_3","T_4","T_5","T_6","T_7")
setnames(data, old, new, skip_absent = TRUE)

data=data[order(Date,Hour)]

str(data)

# data[Date=="2021-02-12"&Hour==23,Consumption:=33000]

# data[Date=="2021-02-05"& Hour==14,Consumption:=(39928.35+44340.30)/2]
# data[Date=="2021-02-05"& Hour==15,Consumption:=(39238.29+43633.22)/2]
# data[Date=="2021-02-05"& Hour==16,Consumption:=(38922.72+43084.56)/2]
# data[Date=="2021-02-05"& Hour==17,Consumption:=(39044.45+42706.24)/2]
# data[Date=="2021-02-05"& Hour==18,Consumption:=(39240.48+42467.46)/2]
# data[Date=="2021-02-05"& Hour==19,Consumption:=(39282.05+41660.61)/2]
# data[Date=="2021-02-05"& Hour==20,Consumption:=(38133.31+40046.77)/2]
# data[Date=="2021-02-05"& Hour==21,Consumption:=(37249.31+39212.66)/2]
# data[Date=="2021-02-05"& Hour==22,Consumption:=(36663.95+38521.08)/2]
# data[Date=="2021-02-05"& Hour==23,Consumption:=(35547.44+37520.36)/2]

# data[Date=="2021-02-05"& Hour==13,Consumption:=(38999.00)]
# data[Date=="2021-02-05"& Hour==14,Consumption:=(39928.35)]
# data[Date=="2021-02-05"& Hour==15,Consumption:=(39238.29)]
# data[Date=="2021-02-05"& Hour==16,Consumption:=(38922.72)]
# data[Date=="2021-02-05"& Hour==17,Consumption:=(39044.45)]
# data[Date=="2021-02-05"& Hour==18,Consumption:=(39240.48)]
# data[Date=="2021-02-05"& Hour==19,Consumption:=(39282.05)]
# data[Date=="2021-02-05"& Hour==20,Consumption:=(38133.31)]
# data[Date=="2021-02-05"& Hour==21,Consumption:=(37249.31)]
# data[Date=="2021-02-05"& Hour==22,Consumption:=(36663.95)]
# data[Date=="2021-02-05"& Hour==23,Consumption:=(35547.44)]

all_data=rbind(dt,data)
summary(all_data)

cont=copy(all_data[,c(1,2,3)])

old <- c("Date", "Hour", "Consumption")
new <- c("date", "hour", "consumption")

setnames(cont, old, new, skip_absent = TRUE)

train=cont[date<=Sys.Date()]
test=cont[date==Sys.Date()+1]

# train=cont[date<"2021-01-09"]
# test=cont[date>="2021-01-09"& date<="2021-01-22"]

names_train<-paste0("hour",0:23,"_train")
names_test<-paste0("hour",0:23,"_test")

train_split <- split(train, train$hour)
new_names_train <- names_train
for (i in 1:length(train_split)) {
    assign(new_names_train[i], train_split[[i]])
}

test_split <- split(test, test$hour)
new_names_test <- names_test
for (i in 1:length(test_split)) {
    assign(new_names_test[i], test_split[[i]])
}

lag_creator<-function(DT, names){   
    hours=c(0:23)
    for (which_hour in hours){
        new_col<-paste0(names,which_hour)
        which_hour=which_hour+1
        DT[,(new_col):=shift(DT[,2],type="lag",n=which_hour)]                
        
    }  
    return(DT[])
}

col_name<-function(DT, names){   
    hours=c(0:23)
    for (which_hour in hours){
        new_col<-paste0(names,which_hour)
        setnames(DT,old=as.character(which_hour),new=as.character(new_col))              
    }  
}

data_wide <- spread(cont, hour, consumption)
lag_2=copy(data_wide[,date:=date+2])
col_name(lag_2,"Lag_day2_hour_")

data_wide <- spread(cont, hour, consumption)
lag_7=copy(data_wide[,date:=date+7])
col_name(lag_7,"Lag_day7_hour_")

dt_with_lag2=merge(cont,lag_2,by.x="date",by.y="date",all.x=TRUE)

full_dt=merge(dt_with_lag2,lag_7,by.x="date",by.y="date",all.x=TRUE)

all_feature=read.csv("C:/Users/bahad/GitHub/360project/all_features_with_dummies.csv")
#all_feature=all_feature[,c(1,5,6,7,8,9,10)]

all_feature$date=as.Date(all_feature$date)
all_feature=all_feature[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,26:36)]
str(all_feature)

full_dt=merge(full_dt,all_feature,by.x="date",by.y="date",all.x=TRUE)

full_dt=merge(full_dt, all_data[,-3], by.x=c("date", "hour"), by.y=c("Date", "Hour"))

hourly_feat_new=fread("hourly_feature_df.csv")
str(hourly_feat_new)

data

full_dt$lockdown_partially=hourly_feat_new[,"is_lockdown"]
full_dt$sun_hourly=hourly_feat_new[,"is_sun"]

full_dt %>% relocate(consumption, .after = last_col())

train_d=filter(full_dt,date<=Sys.Date())
#train

test_d=filter(full_dt,date==Sys.Date()+1)



# train_d=filter(full_dt,date<"2021-01-09")
# #train

# test_d=filter(full_dt,date>="2021-01-09"& date<="2021-01-22")

names_train<-paste0("train_d",0:23)
names_test<-paste0("test_d",0:23)

train_split <- split(train_d, train_d$hour)
new_names_train <- names_train
for (i in 1:length(train_split)) {
    assign(new_names_train[i], train_split[[i]])
}

test_split <- split(test_d, test_d$hour)
new_names_test <- names_test
for (i in 1:length(test_split)) {
    assign(new_names_test[i], test_split[[i]])
}

mape_result_d=rep(0,24)

set.seed(1)

train_mat0=as.matrix(train_d0[complete.cases(train_d0),-c('date',"hour","consumption"),with=F])
train_mat1=as.matrix(train_d1[complete.cases(train_d1),-c('date',"hour","consumption"),with=F])
train_mat2=as.matrix(train_d2[complete.cases(train_d2),-c('date',"hour","consumption"),with=F])
train_mat3=as.matrix(train_d3[complete.cases(train_d3),-c('date',"hour","consumption"),with=F])
train_mat4=as.matrix(train_d4[complete.cases(train_d4),-c('date',"hour","consumption"),with=F])
train_mat5=as.matrix(train_d5[complete.cases(train_d5),-c('date',"hour","consumption"),with=F])
train_mat6=as.matrix(train_d6[complete.cases(train_d6),-c('date',"hour","consumption"),with=F])
train_mat7=as.matrix(train_d7[complete.cases(train_d7),-c('date',"hour","consumption"),with=F])
train_mat8=as.matrix(train_d8[complete.cases(train_d8),-c('date',"hour","consumption"),with=F])
train_mat9=as.matrix(train_d9[complete.cases(train_d9),-c('date',"hour","consumption"),with=F])
train_mat10=as.matrix(train_d10[complete.cases(train_d10),-c('date',"hour","consumption"),with=F])
train_mat11=as.matrix(train_d11[complete.cases(train_d11),-c('date',"hour","consumption"),with=F])
train_mat12=as.matrix(train_d12[complete.cases(train_d12),-c('date',"hour","consumption"),with=F])
train_mat13=as.matrix(train_d13[complete.cases(train_d13),-c('date',"hour","consumption"),with=F])
train_mat14=as.matrix(train_d14[complete.cases(train_d14),-c('date',"hour","consumption"),with=F])
train_mat15=as.matrix(train_d15[complete.cases(train_d15),-c('date',"hour","consumption"),with=F])
train_mat16=as.matrix(train_d16[complete.cases(train_d16),-c('date',"hour","consumption"),with=F])
train_mat17=as.matrix(train_d17[complete.cases(train_d17),-c('date',"hour","consumption"),with=F])
train_mat18=as.matrix(train_d18[complete.cases(train_d18),-c('date',"hour","consumption"),with=F])
train_mat19=as.matrix(train_d19[complete.cases(train_d19),-c('date',"hour","consumption"),with=F])
train_mat20=as.matrix(train_d20[complete.cases(train_d20),-c('date',"hour","consumption"),with=F])
train_mat21=as.matrix(train_d21[complete.cases(train_d21),-c('date',"hour","consumption"),with=F])
train_mat22=as.matrix(train_d22[complete.cases(train_d22),-c('date',"hour","consumption"),with=F])
train_mat23=as.matrix(train_d23[complete.cases(train_d23),-c('date',"hour","consumption"),with=F])

result_vec0=as.vector(t(train_d0[complete.cases(train_d0),"consumption"]))
result_vec1=as.vector(t(train_d1[complete.cases(train_d1),"consumption"]))
result_vec2=as.vector(t(train_d2[complete.cases(train_d2),"consumption"]))
result_vec3=as.vector(t(train_d3[complete.cases(train_d3),"consumption"]))
result_vec4=as.vector(t(train_d4[complete.cases(train_d4),"consumption"]))
result_vec5=as.vector(t(train_d5[complete.cases(train_d5),"consumption"]))
result_vec6=as.vector(t(train_d6[complete.cases(train_d6),"consumption"]))
result_vec7=as.vector(t(train_d7[complete.cases(train_d7),"consumption"]))
result_vec8=as.vector(t(train_d8[complete.cases(train_d8),"consumption"]))
result_vec9=as.vector(t(train_d9[complete.cases(train_d9),"consumption"]))
result_vec10=as.vector(t(train_d10[complete.cases(train_d10),"consumption"]))
result_vec11=as.vector(t(train_d11[complete.cases(train_d11),"consumption"]))
result_vec12=as.vector(t(train_d12[complete.cases(train_d12),"consumption"]))
result_vec13=as.vector(t(train_d13[complete.cases(train_d13),"consumption"]))
result_vec14=as.vector(t(train_d14[complete.cases(train_d14),"consumption"]))
result_vec15=as.vector(t(train_d15[complete.cases(train_d15),"consumption"]))
result_vec16=as.vector(t(train_d16[complete.cases(train_d16),"consumption"]))
result_vec17=as.vector(t(train_d17[complete.cases(train_d17),"consumption"]))
result_vec18=as.vector(t(train_d18[complete.cases(train_d18),"consumption"]))
result_vec19=as.vector(t(train_d19[complete.cases(train_d19),"consumption"]))
result_vec20=as.vector(t(train_d20[complete.cases(train_d20),"consumption"]))
result_vec21=as.vector(t(train_d21[complete.cases(train_d21),"consumption"]))
result_vec22=as.vector(t(train_d22[complete.cases(train_d22),"consumption"]))
result_vec23=as.vector(t(train_d23[complete.cases(train_d23),"consumption"]))

cvfit0=cv.glmnet(train_mat0,result_vec0,family='gaussian',nfolds = 10,type.measure="mae")
cvfit1=cv.glmnet(train_mat1,result_vec1,family='gaussian',nfolds = 10,type.measure="mae")
cvfit2=cv.glmnet(train_mat2,result_vec2,family='gaussian',nfolds = 10,type.measure="mae")
cvfit3=cv.glmnet(train_mat3,result_vec3,family='gaussian',nfolds = 10,type.measure="mae")
cvfit4=cv.glmnet(train_mat4,result_vec4,family='gaussian',nfolds = 10,type.measure="mae")
cvfit5=cv.glmnet(train_mat5,result_vec5,family='gaussian',nfolds = 10,type.measure="mae")
cvfit6=cv.glmnet(train_mat6,result_vec6,family='gaussian',nfolds = 10,type.measure="mae")
cvfit7=cv.glmnet(train_mat7,result_vec7,family='gaussian',nfolds = 10,type.measure="mae")
cvfit8=cv.glmnet(train_mat8,result_vec8,family='gaussian',nfolds = 10,type.measure="mae")
cvfit9=cv.glmnet(train_mat9,result_vec9,family='gaussian',nfolds = 10,type.measure="mae")
cvfit10=cv.glmnet(train_mat10,result_vec10,family='gaussian',nfolds = 10,type.measure="mae")
cvfit11=cv.glmnet(train_mat11,result_vec11,family='gaussian',nfolds = 10,type.measure="mae")
cvfit12=cv.glmnet(train_mat12,result_vec12,family='gaussian',nfolds = 10,type.measure="mae")
cvfit13=cv.glmnet(train_mat13,result_vec13,family='gaussian',nfolds = 10,type.measure="mae")
cvfit14=cv.glmnet(train_mat14,result_vec14,family='gaussian',nfolds = 10,type.measure="mae")
cvfit15=cv.glmnet(train_mat15,result_vec15,family='gaussian',nfolds = 10,type.measure="mae")
cvfit16=cv.glmnet(train_mat16,result_vec16,family='gaussian',nfolds = 10,type.measure="mae")
cvfit17=cv.glmnet(train_mat17,result_vec17,family='gaussian',nfolds = 10,type.measure="mae")
cvfit18=cv.glmnet(train_mat18,result_vec18,family='gaussian',nfolds = 10,type.measure="mae")
cvfit19=cv.glmnet(train_mat19,result_vec19,family='gaussian',nfolds = 10,type.measure="mae")
cvfit20=cv.glmnet(train_mat20,result_vec20,family='gaussian',nfolds = 10,type.measure="mae")
cvfit21=cv.glmnet(train_mat21,result_vec21,family='gaussian',nfolds = 10,type.measure="mae")
cvfit22=cv.glmnet(train_mat22,result_vec22,family='gaussian',nfolds = 10,type.measure="mae")
cvfit23=cv.glmnet(train_mat23,result_vec23,family='gaussian',nfolds = 10,type.measure="mae")

test_mat0=as.matrix(test_d0[complete.cases(test_d0),-c('date',"hour","consumption")])
test_mat1=as.matrix(test_d1[complete.cases(test_d1),-c('date',"hour","consumption")])
test_mat2=as.matrix(test_d2[complete.cases(test_d2),-c('date',"hour","consumption")])
test_mat3=as.matrix(test_d3[complete.cases(test_d3),-c('date',"hour","consumption")])
test_mat4=as.matrix(test_d4[complete.cases(test_d4),-c('date',"hour","consumption")])
test_mat5=as.matrix(test_d5[complete.cases(test_d5),-c('date',"hour","consumption")])
test_mat6=as.matrix(test_d6[complete.cases(test_d6),-c('date',"hour","consumption")])
test_mat7=as.matrix(test_d7[complete.cases(test_d7),-c('date',"hour","consumption")])
test_mat8=as.matrix(test_d8[complete.cases(test_d8),-c('date',"hour","consumption")])
test_mat9=as.matrix(test_d9[complete.cases(test_d9),-c('date',"hour","consumption")])
test_mat10=as.matrix(test_d10[complete.cases(test_d10),-c('date',"hour","consumption")])
test_mat11=as.matrix(test_d11[complete.cases(test_d11),-c('date',"hour","consumption")])
test_mat12=as.matrix(test_d12[complete.cases(test_d12),-c('date',"hour","consumption")])
test_mat13=as.matrix(test_d13[complete.cases(test_d13),-c('date',"hour","consumption")])
test_mat14=as.matrix(test_d14[complete.cases(test_d14),-c('date',"hour","consumption")])
test_mat15=as.matrix(test_d15[complete.cases(test_d15),-c('date',"hour","consumption")])
test_mat16=as.matrix(test_d16[complete.cases(test_d16),-c('date',"hour","consumption")])
test_mat17=as.matrix(test_d17[complete.cases(test_d17),-c('date',"hour","consumption")])
test_mat18=as.matrix(test_d18[complete.cases(test_d18),-c('date',"hour","consumption")])
test_mat19=as.matrix(test_d19[complete.cases(test_d19),-c('date',"hour","consumption")])
test_mat20=as.matrix(test_d20[complete.cases(test_d20),-c('date',"hour","consumption")])
test_mat21=as.matrix(test_d21[complete.cases(test_d21),-c('date',"hour","consumption")])
test_mat22=as.matrix(test_d22[complete.cases(test_d22),-c('date',"hour","consumption")])
test_mat23=as.matrix(test_d23[complete.cases(test_d23),-c('date',"hour","consumption")])

lasso_model0 <- glmnet(train_mat0,result_vec0, alpha = 1, lambda = cvfit0$lambda.min, standardize = FALSE)
lasso_model1 <- glmnet(train_mat1,result_vec1, alpha = 1, lambda = cvfit1$lambda.min, standardize = FALSE)
lasso_model2 <- glmnet(train_mat2,result_vec2, alpha = 1, lambda = cvfit2$lambda.min, standardize = FALSE)
lasso_model3 <- glmnet(train_mat3,result_vec3, alpha = 1, lambda = cvfit3$lambda.min, standardize = FALSE)
lasso_model4 <- glmnet(train_mat4,result_vec4, alpha = 1, lambda = cvfit4$lambda.min, standardize = FALSE)
lasso_model5 <- glmnet(train_mat5,result_vec5, alpha = 1, lambda = cvfit5$lambda.min, standardize = FALSE)
lasso_model6 <- glmnet(train_mat6,result_vec6, alpha = 1, lambda = cvfit6$lambda.min, standardize = FALSE)
lasso_model7 <- glmnet(train_mat7,result_vec7, alpha = 1, lambda = cvfit7$lambda.min, standardize = FALSE)
lasso_model8 <- glmnet(train_mat8,result_vec8, alpha = 1, lambda = cvfit8$lambda.min, standardize = FALSE)
lasso_model9 <- glmnet(train_mat9,result_vec9, alpha = 1, lambda = cvfit9$lambda.min, standardize = FALSE)
lasso_model10 <- glmnet(train_mat10,result_vec10, alpha = 1, lambda = cvfit10$lambda.min, standardize = FALSE)
lasso_model11 <- glmnet(train_mat11,result_vec11, alpha = 1, lambda = cvfit11$lambda.min, standardize = FALSE)
lasso_model12 <- glmnet(train_mat12,result_vec12, alpha = 1, lambda = cvfit12$lambda.min, standardize = FALSE)
lasso_model13 <- glmnet(train_mat13,result_vec13, alpha = 1, lambda = cvfit13$lambda.min, standardize = FALSE)
lasso_model14 <- glmnet(train_mat14,result_vec14, alpha = 1, lambda = cvfit14$lambda.min, standardize = FALSE)
lasso_model15 <- glmnet(train_mat15,result_vec15, alpha = 1, lambda = cvfit15$lambda.min, standardize = FALSE)
lasso_model16 <- glmnet(train_mat16,result_vec16, alpha = 1, lambda = cvfit16$lambda.min, standardize = FALSE)
lasso_model17 <- glmnet(train_mat17,result_vec17, alpha = 1, lambda = cvfit17$lambda.min, standardize = FALSE)
lasso_model18 <- glmnet(train_mat18,result_vec18, alpha = 1, lambda = cvfit18$lambda.min, standardize = FALSE)
lasso_model19 <- glmnet(train_mat19,result_vec19, alpha = 1, lambda = cvfit19$lambda.min, standardize = FALSE)
lasso_model20 <- glmnet(train_mat20,result_vec20, alpha = 1, lambda = cvfit20$lambda.min, standardize = FALSE)
lasso_model21 <- glmnet(train_mat21,result_vec21, alpha = 1, lambda = cvfit21$lambda.min, standardize = FALSE)
lasso_model22 <- glmnet(train_mat22,result_vec22, alpha = 1, lambda = cvfit22$lambda.min, standardize = FALSE)
lasso_model23 <- glmnet(train_mat23,result_vec23, alpha = 1, lambda = cvfit23$lambda.min, standardize = FALSE)

predicts_hour0 <- predict(lasso_model0, s = cvfit0$lambda.min, newx = test_mat0)
predicts_hour1 <- predict(lasso_model1, s = cvfit1$lambda.min, newx = test_mat1)
predicts_hour2 <- predict(lasso_model2, s = cvfit2$lambda.min, newx = test_mat2)
predicts_hour3 <- predict(lasso_model3, s = cvfit3$lambda.min, newx = test_mat3)
predicts_hour4 <- predict(lasso_model4, s = cvfit4$lambda.min, newx = test_mat4)
predicts_hour5 <- predict(lasso_model5, s = cvfit5$lambda.min, newx = test_mat5)
predicts_hour6 <- predict(lasso_model6, s = cvfit6$lambda.min, newx = test_mat6)
predicts_hour7 <- predict(lasso_model7, s = cvfit7$lambda.min, newx = test_mat7)
predicts_hour8 <- predict(lasso_model8, s = cvfit8$lambda.min, newx = test_mat8)
predicts_hour9 <- predict(lasso_model9, s = cvfit9$lambda.min, newx = test_mat9)
predicts_hour10 <- predict(lasso_model10, s = cvfit10$lambda.min, newx = test_mat10)
predicts_hour11 <- predict(lasso_model11, s = cvfit11$lambda.min, newx = test_mat11)
predicts_hour12 <- predict(lasso_model12, s = cvfit12$lambda.min, newx = test_mat12)
predicts_hour13 <- predict(lasso_model13, s = cvfit13$lambda.min, newx = test_mat13)
predicts_hour14 <- predict(lasso_model14, s = cvfit14$lambda.min, newx = test_mat14)
predicts_hour15 <- predict(lasso_model15, s = cvfit15$lambda.min, newx = test_mat15)
predicts_hour16 <- predict(lasso_model16, s = cvfit16$lambda.min, newx = test_mat16)
predicts_hour17 <- predict(lasso_model17, s = cvfit17$lambda.min, newx = test_mat17)
predicts_hour18 <- predict(lasso_model18, s = cvfit18$lambda.min, newx = test_mat18)
predicts_hour19 <- predict(lasso_model19, s = cvfit19$lambda.min, newx = test_mat19)
predicts_hour20 <- predict(lasso_model20, s = cvfit20$lambda.min, newx = test_mat20)
predicts_hour21 <- predict(lasso_model21, s = cvfit21$lambda.min, newx = test_mat21)
predicts_hour22 <- predict(lasso_model22, s = cvfit22$lambda.min, newx = test_mat22)
predicts_hour23 <- predict(lasso_model23, s = cvfit23$lambda.min, newx = test_mat23)

predictions_all=data.table(date=unique(test$date))

for(i in 0:23){
    predictions_all=cbind(predictions_all,get(paste0("predicts_hour",i)))
    colnames(predictions_all)[i+2] <- paste0(i)
}

library(reshape2)

predictions_all_long=melt(predictions_all, id.vars=c("date"))
predictions_all_long$variable=as.numeric(predictions_all_long$variable)-1
setnames(predictions_all_long,"variable", "hour")
setnames(predictions_all_long,"value", "consumption")
predictions_all_long=predictions_all_long[order(date,hour)]

res_dt=data.table(date=test$date,
                 hour=test$hour,
                 actual=test$consumption,
                 forecasted=predictions_all_long$consumption)

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

data[, mean(Consumption), by=Date]

# res_dt$forecasted=res_dt$forecasted+200
# res_dt

res_dt

perf_dt("Linear Regression with Lasso Penalty", res_dt$actual, res_dt$forecasted)

submit_now = TRUE
predictions=data.table(Date=rep(as.Date(Sys.time())+1,24),Hour=0:23)
# be sure if ordered
predictions=predictions[order(Date,Hour)]
# dummy forecast
predictions[,forecast:=res_dt$forecasted]

send_submission(predictions, token, url=subm_url, submit_now=T)





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
library(lubridate, quietly=TRUE)
library(zoo, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(glmnet)
library(MLmetrics)
library("mvtnorm") 
library(tidyr)
library(ggplot2)

dt=fread("bulk_consumption_with_temp.csv")

str(dt)

str(data)

setnames(data, "event_date", "Date")
setnames(data, "event_hour", "Hour")
setnames(data, "consumption", "Consumption")
setnames(data, "t_1", "T_1")
setnames(data, "t_2", "T_2")
setnames(data, "t_3", "T_3")
setnames(data, "t_4", "T_4")
setnames(data, "t_5", "T_5")
setnames(data, "t_6", "T_6")
setnames(data, "t_7", "T_7")

data$Date=as.Date(data$Date)
dt$Date=as.Date(dt$Date)

data=rbind(dt,data)

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

dt_lagged=copy(data)
dt_lagged[, Lag_48:=shift(Consumption,type="lag",n=48)]
dt_lagged[, Lag_168:=shift(Consumption,type="lag",n=168)]

train=filter(dt_lagged,Date<"2021-01-08")

test=filter(dt_lagged,Date>"2021-01-08" & Date<="2021-01-22")

head(train)

perf_dt("Lag 48 for hourly prediction", test$Consumption, test$Lag_48)
perf_dt("Lag 168 for hourly prediction", test$Consumption, test$Lag_168)

test %<>% 
  group_by(Date) %>% 
  summarise(Consumption = sum(Consumption),
            Lag_2 = sum(Lag_48),
            Lag_7= sum(Lag_168)
           )

perf_dt("Lag 2 for daily prediction", test$Consumption, test$Lag_2)
perf_dt("Lag 7 for daily prediction", test$Consumption, test$Lag_7)



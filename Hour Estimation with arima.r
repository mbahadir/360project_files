library(data.table)

feature_dt_hour=fread("C:/Users/bahad/GitHub/360project/hourly_feature_df.csv")
feature_dt_hour=feature_dt_hour[,-1]
feature_dt_hour$date=as.Date(feature_dt_hour$date)

req_feature_hour=feature_dt_hour[date>"2020-06-06"&date<=Sys.Date()-8]

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
  assign(paste0("hour",i), time_cons[Hour==i & date>"2020-06-06"&date<=Sys.Date()-8,])
  }

for(i in 0:23){
    #plot(get(paste0("hour",i))[date>"2020-06-01"&date<=Sys.Date()-8]$perc,type="l")
    assign(paste0("decompose",i),decompose(ts(get(paste0("hour",i))$perc,freq=7),
                                           type='add'))
    #plot(get(paste0("decompose",i)))
}

req_feature_hour$hour=rep(0:23,nrow(req_feature_hour)/24)

library(forecast)

res_hour_per=data.table(hour=0:23)

results_percentage=vector("list",24)

for(i in 0:23){
    new_forescasted=0
    fit_mod=auto.arima(get(paste0("decompose",i))$random)
    new_forescasted=as.numeric(predict(fit_mod, n.ahead = 1)$pred +
                               get(paste0("decompose",i))$season[wday(Sys.Date())] +
                                     tail(get(paste0("decompose",i))$trend[!is.na(get(paste0("decompose",i))$trend)],1))
    results_percentage[[i+1]]=new_forescasted
}    



Percentage_dt=t(rbind(results_percentage))





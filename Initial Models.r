library(data.table)
library(dplyr)
library(tidyr)
library(MLmetrics)

dt=fread("bulk_consumption_with_temp.csv")

dt_new=fread("all_features_df.csv")
droped=fread("droped_all_features_df.csv")

dt$Date=as.Date(dt$Date)
str(dt)

str(dt_new)

old <- c("holiday_val(as_factor)", "weekday(as_factor)", "lock_down(as_factor)")
new <- c("holiday_val", "weekday", "lock_down")

setnames(dt_new, old, new, skip_absent = TRUE)

dt_new$date=as.Date(dt_new$date)
#dt_new$holiday_val=as.factor(dt_new$holiday_val)
dt_new$weekday=as.factor(dt_new$weekday)
dt_new$lock_down=as.factor(dt_new$lock_down)
str(dt_new)

form_pca <-prcomp(dt[,c(3,4,5,6,7,8,9)], center = TRUE,scale. = TRUE)
form_pca
summary(form_pca)

dt=mutate(dt, reduced_temp= 0.3794502 * T_1 + 0.3748089 * T_2 + 0.3782977 * T_3 + 0.3800144 * T_4 + 0.3790882 * T_5 + 0.3803995 * T_6 + 0.3736363 * T_7)

tail(dt)

dt_new=merge(dt, dt_new, by.x="Date", by.y="date")

dt_new[,T_1:=shift(T_1,type="lag",n=2)]
dt_new[,T_2:=shift(T_2,type="lag",n=2)]
dt_new[,T_3:=shift(T_3,type="lag",n=2)]
dt_new[,T_4:=shift(T_4,type="lag",n=2)]
dt_new[,T_5:=shift(T_5,type="lag",n=2)]
dt_new[,T_6:=shift(T_6,type="lag",n=2)]
dt_new[,T_7:=shift(T_7,type="lag",n=2)]
dt_new[,reduced_temp:=shift(reduced_temp,type="lag",n=2)]
dt_new[,covid_severity:=shift(covid_severity,type="lag",n=2)]
dt_new[,sunlight_time_minutes:=shift(sunlight_time_minutes,type="lag",n=2)]

dt_new

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

str(dt_new)

dt_new[, day_2:=shift(Consumption,type="lag",n=2)]
dt_new[, day_7:=shift(Consumption,type="lag",n=7)]

train=filter(dt,Date<"2020-12-01")
test=filter(dt,Date>="2020-12-01")

train_daily=filter(dt_new,Date<"2020-12-01")
test_daily=filter(dt_new,Date>="2020-12-01")

train_daily

fit_daily=lm(Consumption~.-Date,data=train_daily)
summary(fit_daily)

fit_daily_step=step(fit_daily)
summary(fit_daily_step)

first_pred_daily=predict(fit_daily_step,newdata=test_daily)
perf_dt("First Logistic Regression(daily)",test_daily$Consumption, first_pred_daily)

library(glmnet)

cont=copy(dt[,c(1,2,3)])

old <- c("Date", "Hour", "Consumption")
new <- c("date", "hour", "consumption")

setnames(cont, old, new, skip_absent = TRUE)

old <- c("Date", "Hour", "Consumption")
new <- c("date", "hour", "consumption")

setnames(train, old, new, skip_absent = TRUE)

old <- c("Date", "Hour", "Consumption")
new <- c("date", "hour", "consumption")

setnames(test, old, new, skip_absent = TRUE)

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

feature_mat<-function(DT,names){
    feat_mat=data.table(date=seq.POSIXt(from = as.POSIXct("2016-01-01"),to = as.POSIXct("2020-12-01"),by="day"))
    hours=c(0:23)    
    for (which_hour in hours){
    new_col<-paste0(names,which_hour)
    feat_mat[,(new_col):=0]                
    } 
    for(whic_date in seq.POSIXt(from = as.POSIXct("2016-01-01"),to = as.POSIXct("2020-12-01"),by="day")){
        current_DT=filter(DT,date==as.POSIXct(whic_date, origin = "1970-01-01"))
        print(current_DT)
        for(which_hour in hours){
            feat_mat[date==as.POSIXct(whic_date, origin = "1970-01-01"), 
                     feat_mat[,which_hour+1]:=current_DT[hour==which_hour,consumption]] 
        }   
    }
    return(feat_mat[])
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
#lag_2

data_wide <- spread(cont, hour, consumption)
lag_7=copy(data_wide[,date:=date+7])
col_name(lag_7,"Lag_day7_hour_")
#lag_7

dt_with_lag2=merge(cont,lag_2,by.x="date",by.y="date",all.x=TRUE)

full_dt=merge(dt_with_lag2,lag_7,by.x="date",by.y="date",all.x=TRUE)

library(glmnet)

full_dt %>% relocate(consumption, .after = last_col())
#full_dt

str(dt_new)

dt_new

setnames(dt_new, "Date", "date")
setnames(dt_new, "Hour", "hour")

full_dt_new=cbind(full_dt,dt_new[,c(-1,-2,-3)])

filter(full_dt_new,date<"2020-12-01")

train_d=filter(full_dt,date<"2020-12-01")
#train

test_d=filter(full_dt,date>="2020-12-01")
#test

test_d

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

#print(cvfit$name)
#print(cvfit$glmnet.fit)
#print(cvfit$lambda.1se)
#print(cvfit$lambda.min)
#plot(cvfit)
#coef(cvfit,s="lambda.min")
#coef(cvfit,s="lambda.1se")

mape_result_d=rep(0,24)

set.seed(1)

train_mat0=data.matrix(train_d0[complete.cases(train_d0),-c('date',"hour","consumption"),with=F])
train_mat1=data.matrix(train_d1[complete.cases(train_d1),-c('date',"hour","consumption"),with=F])
train_mat2=data.matrix(train_d2[complete.cases(train_d2),-c('date',"hour","consumption"),with=F])
train_mat3=data.matrix(train_d3[complete.cases(train_d3),-c('date',"hour","consumption"),with=F])
train_mat4=data.matrix(train_d4[complete.cases(train_d4),-c('date',"hour","consumption"),with=F])
train_mat5=data.matrix(train_d5[complete.cases(train_d5),-c('date',"hour","consumption"),with=F])
train_mat6=data.matrix(train_d6[complete.cases(train_d6),-c('date',"hour","consumption"),with=F])
train_mat7=data.matrix(train_d7[complete.cases(train_d7),-c('date',"hour","consumption"),with=F])
train_mat8=data.matrix(train_d8[complete.cases(train_d8),-c('date',"hour","consumption"),with=F])
train_mat9=data.matrix(train_d9[complete.cases(train_d9),-c('date',"hour","consumption"),with=F])
train_mat10=data.matrix(train_d10[complete.cases(train_d10),-c('date',"hour","consumption"),with=F])
train_mat11=data.matrix(train_d11[complete.cases(train_d11),-c('date',"hour","consumption"),with=F])
train_mat12=data.matrix(train_d12[complete.cases(train_d12),-c('date',"hour","consumption"),with=F])
train_mat13=data.matrix(train_d13[complete.cases(train_d13),-c('date',"hour","consumption"),with=F])
train_mat14=data.matrix(train_d14[complete.cases(train_d14),-c('date',"hour","consumption"),with=F])
train_mat15=data.matrix(train_d15[complete.cases(train_d15),-c('date',"hour","consumption"),with=F])
train_mat16=data.matrix(train_d16[complete.cases(train_d16),-c('date',"hour","consumption"),with=F])
train_mat17=data.matrix(train_d17[complete.cases(train_d17),-c('date',"hour","consumption"),with=F])
train_mat18=data.matrix(train_d18[complete.cases(train_d18),-c('date',"hour","consumption"),with=F])
train_mat19=data.matrix(train_d19[complete.cases(train_d19),-c('date',"hour","consumption"),with=F])
train_mat20=data.matrix(train_d20[complete.cases(train_d20),-c('date',"hour","consumption"),with=F])
train_mat21=data.matrix(train_d21[complete.cases(train_d21),-c('date',"hour","consumption"),with=F])
train_mat22=data.matrix(train_d22[complete.cases(train_d22),-c('date',"hour","consumption"),with=F])
train_mat23=data.matrix(train_d23[complete.cases(train_d23),-c('date',"hour","consumption"),with=F])

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

cvfit0=cv.glmnet(train_mat0,result_vec0,family='gaussian',nfolds = 10,type.measure="mse")
cvfit1=cv.glmnet(train_mat1,result_vec1,family='gaussian',nfolds = 10,type.measure="mse")
cvfit2=cv.glmnet(train_mat2,result_vec2,family='gaussian',nfolds = 10,type.measure="mse")
cvfit3=cv.glmnet(train_mat3,result_vec3,family='gaussian',nfolds = 10,type.measure="mse")
cvfit4=cv.glmnet(train_mat4,result_vec4,family='gaussian',nfolds = 10,type.measure="mse")
cvfit5=cv.glmnet(train_mat5,result_vec5,family='gaussian',nfolds = 10,type.measure="mse")
cvfit6=cv.glmnet(train_mat6,result_vec6,family='gaussian',nfolds = 10,type.measure="mse")
cvfit7=cv.glmnet(train_mat7,result_vec7,family='gaussian',nfolds = 10,type.measure="mse")
cvfit8=cv.glmnet(train_mat8,result_vec8,family='gaussian',nfolds = 10,type.measure="mse")
cvfit9=cv.glmnet(train_mat9,result_vec9,family='gaussian',nfolds = 10,type.measure="mse")
cvfit10=cv.glmnet(train_mat10,result_vec10,family='gaussian',nfolds = 10,type.measure="mse")
cvfit11=cv.glmnet(train_mat11,result_vec11,family='gaussian',nfolds = 10,type.measure="mse")
cvfit12=cv.glmnet(train_mat12,result_vec12,family='gaussian',nfolds = 10,type.measure="mse")
cvfit13=cv.glmnet(train_mat13,result_vec13,family='gaussian',nfolds = 10,type.measure="mse")
cvfit14=cv.glmnet(train_mat14,result_vec14,family='gaussian',nfolds = 10,type.measure="mse")
cvfit15=cv.glmnet(train_mat15,result_vec15,family='gaussian',nfolds = 10,type.measure="mse")
cvfit16=cv.glmnet(train_mat16,result_vec16,family='gaussian',nfolds = 10,type.measure="mse")
cvfit17=cv.glmnet(train_mat17,result_vec17,family='gaussian',nfolds = 10,type.measure="mse")
cvfit18=cv.glmnet(train_mat18,result_vec18,family='gaussian',nfolds = 10,type.measure="mse")
cvfit19=cv.glmnet(train_mat19,result_vec19,family='gaussian',nfolds = 10,type.measure="mse")
cvfit20=cv.glmnet(train_mat20,result_vec20,family='gaussian',nfolds = 10,type.measure="mse")
cvfit21=cv.glmnet(train_mat21,result_vec21,family='gaussian',nfolds = 10,type.measure="mse")
cvfit22=cv.glmnet(train_mat22,result_vec22,family='gaussian',nfolds = 10,type.measure="mse")
cvfit23=cv.glmnet(train_mat23,result_vec23,family='gaussian',nfolds = 10,type.measure="mse")

#for(i in seq(0,23,by=1)){    
#    plot(get(paste0("cvfit",i)))
#    print(paste("Hour",i))
#    print(paste("Min value for Lambda=",get(paste0("cvfit",i))$lambda.min))
#    print(paste("1se value for Lambda=",get(paste0("cvfit",i))$lambda.1se))    
#    print(coef(get(paste0("cvfit",i)), s = 'lambda.min'))
#    print(paste0("Plot for ",i,"'th hour's Cross Validation"))
#}

test_mat0=data.matrix(test_d0[complete.cases(test_d0),-c('date',"hour","consumption")])
test_mat1=data.matrix(test_d1[complete.cases(test_d1),-c('date',"hour","consumption")])
test_mat2=data.matrix(test_d2[complete.cases(test_d2),-c('date',"hour","consumption")])
test_mat3=data.matrix(test_d3[complete.cases(test_d3),-c('date',"hour","consumption")])
test_mat4=data.matrix(test_d4[complete.cases(test_d4),-c('date',"hour","consumption")])
test_mat5=data.matrix(test_d5[complete.cases(test_d5),-c('date',"hour","consumption")])
test_mat6=data.matrix(test_d6[complete.cases(test_d6),-c('date',"hour","consumption")])
test_mat7=data.matrix(test_d7[complete.cases(test_d7),-c('date',"hour","consumption")])
test_mat8=data.matrix(test_d8[complete.cases(test_d8),-c('date',"hour","consumption")])
test_mat9=data.matrix(test_d9[complete.cases(test_d9),-c('date',"hour","consumption")])
test_mat10=data.matrix(test_d10[complete.cases(test_d10),-c('date',"hour","consumption")])
test_mat11=data.matrix(test_d11[complete.cases(test_d11),-c('date',"hour","consumption")])
test_mat12=data.matrix(test_d12[complete.cases(test_d12),-c('date',"hour","consumption")])
test_mat13=data.matrix(test_d13[complete.cases(test_d13),-c('date',"hour","consumption")])
test_mat14=data.matrix(test_d14[complete.cases(test_d14),-c('date',"hour","consumption")])
test_mat15=data.matrix(test_d15[complete.cases(test_d15),-c('date',"hour","consumption")])
test_mat16=data.matrix(test_d16[complete.cases(test_d16),-c('date',"hour","consumption")])
test_mat17=data.matrix(test_d17[complete.cases(test_d17),-c('date',"hour","consumption")])
test_mat18=data.matrix(test_d18[complete.cases(test_d18),-c('date',"hour","consumption")])
test_mat19=data.matrix(test_d19[complete.cases(test_d19),-c('date',"hour","consumption")])
test_mat20=data.matrix(test_d20[complete.cases(test_d20),-c('date',"hour","consumption")])
test_mat21=data.matrix(test_d21[complete.cases(test_d21),-c('date',"hour","consumption")])
test_mat22=data.matrix(test_d22[complete.cases(test_d22),-c('date',"hour","consumption")])
test_mat23=data.matrix(test_d23[complete.cases(test_d23),-c('date',"hour","consumption")])

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

mape_result_d[1] <- MAPE(test_d0$consumption, predicts_hour0)
mape_result_d[2] <- MAPE(test_d1$consumption, predicts_hour1)
mape_result_d[3] <- MAPE(test_d2$consumption, predicts_hour2)
mape_result_d[4] <- MAPE(test_d3$consumption, predicts_hour3)
mape_result_d[5] <- MAPE(test_d4$consumption, predicts_hour4)
mape_result_d[6] <- MAPE(test_d5$consumption, predicts_hour5)
mape_result_d[7] <- MAPE(test_d6$consumption, predicts_hour6)
mape_result_d[8] <- MAPE(test_d7$consumption, predicts_hour7)
mape_result_d[9] <- MAPE(test_d8$consumption, predicts_hour8)
mape_result_d[10] <- MAPE(test_d9$consumption, predicts_hour9)
mape_result_d[11] <- MAPE(test_d10$consumption, predicts_hour10)
mape_result_d[12] <- MAPE(test_d11$consumption, predicts_hour11)
mape_result_d[13] <- MAPE(test_d12$consumption, predicts_hour12)
mape_result_d[14] <- MAPE(test_d13$consumption, predicts_hour13)
mape_result_d[15] <- MAPE(test_d14$consumption, predicts_hour14)
mape_result_d[16] <- MAPE(test_d15$consumption, predicts_hour15)
mape_result_d[17] <- MAPE(test_d16$consumption, predicts_hour16)
mape_result_d[18] <- MAPE(test_d17$consumption, predicts_hour17)
mape_result_d[19] <- MAPE(test_d18$consumption, predicts_hour18)
mape_result_d[20] <- MAPE(test_d19$consumption, predicts_hour19)
mape_result_d[21] <- MAPE(test_d20$consumption, predicts_hour20)
mape_result_d[22] <- MAPE(test_d21$consumption, predicts_hour21)
mape_result_d[23] <- MAPE(test_d22$consumption, predicts_hour22)
mape_result_d[24] <- MAPE(test_d23$consumption, predicts_hour23)
#mape_result_d

plot(mape_result_d,type="l",ylab="MAPE Values (Quantity)",xlab="Hours",main="MAPE Values for Hourly Model with Lasso Penalty")

MAPE0d_reg_values=as.vector(abs(((hour0_test$consumption-predicts_hour0)/predicts_hour0)))
MAPE1d_reg_values=as.vector(abs(((hour1_test$consumption-predicts_hour1)/predicts_hour1)))
MAPE2d_reg_values=as.vector(abs(((hour2_test$consumption-predicts_hour2)/predicts_hour2)))
MAPE3d_reg_values=as.vector(abs(((hour3_test$consumption-predicts_hour3)/predicts_hour3)))
MAPE4d_reg_values=as.vector(abs(((hour4_test$consumption-predicts_hour4)/predicts_hour4)))
MAPE5d_reg_values=as.vector(abs(((hour5_test$consumption-predicts_hour5)/predicts_hour5)))
MAPE6d_reg_values=as.vector(abs(((hour6_test$consumption-predicts_hour6)/predicts_hour6)))
MAPE7d_reg_values=as.vector(abs(((hour7_test$consumption-predicts_hour7)/predicts_hour7)))
MAPE8d_reg_values=as.vector(abs(((hour8_test$consumption-predicts_hour8)/predicts_hour8)))
MAPE9d_reg_values=as.vector(abs(((hour9_test$consumption-predicts_hour9)/predicts_hour9)))
MAPE10d_reg_values=as.vector(abs(((hour10_test$consumption-predicts_hour10)/predicts_hour10)))
MAPE11d_reg_values=as.vector(abs(((hour11_test$consumption-predicts_hour11)/predicts_hour11)))
MAPE12d_reg_values=as.vector(abs(((hour12_test$consumption-predicts_hour12)/predicts_hour12)))
MAPE13d_reg_values=as.vector(abs(((hour13_test$consumption-predicts_hour13)/predicts_hour13)))
MAPE14d_reg_values=as.vector(abs(((hour14_test$consumption-predicts_hour14)/predicts_hour14)))
MAPE15d_reg_values=as.vector(abs(((hour15_test$consumption-predicts_hour15)/predicts_hour15)))
MAPE16d_reg_values=as.vector(abs(((hour16_test$consumption-predicts_hour16)/predicts_hour16)))
MAPE17d_reg_values=as.vector(abs(((hour17_test$consumption-predicts_hour17)/predicts_hour17)))
MAPE18d_reg_values=as.vector(abs(((hour18_test$consumption-predicts_hour18)/predicts_hour18)))
MAPE19d_reg_values=as.vector(abs(((hour19_test$consumption-predicts_hour19)/predicts_hour19)))
MAPE20d_reg_values=as.vector(abs(((hour20_test$consumption-predicts_hour20)/predicts_hour20)))
MAPE21d_reg_values=as.vector(abs(((hour21_test$consumption-predicts_hour21)/predicts_hour21)))
MAPE22d_reg_values=as.vector(abs(((hour22_test$consumption-predicts_hour22)/predicts_hour22)))
MAPE23d_reg_values=as.vector(abs(((hour23_test$consumption-predicts_hour23)/predicts_hour23)))


D_all_val=c(MAPE0d_reg_values,MAPE1d_reg_values,MAPE2d_reg_values,MAPE3d_reg_values,MAPE4d_reg_values,MAPE5d_reg_values,
           MAPE6d_reg_values,MAPE7d_reg_values,MAPE8d_reg_values,MAPE9d_reg_values,MAPE10d_reg_values,MAPE11d_reg_values,
           MAPE12d_reg_values,MAPE13d_reg_values,MAPE14d_reg_values,MAPE15d_reg_values,MAPE16d_reg_values,MAPE17d_reg_values,
           MAPE18d_reg_values,MAPE19d_reg_values,MAPE20d_reg_values,MAPE21d_reg_values,MAPE22d_reg_values,MAPE23d_reg_values)

lasso_dt=data.table(
                    date=unique(test$date),
                    hour_0=predicts_hour0,hour_1=predicts_hour1,hour_2=predicts_hour2,hour_3=predicts_hour3,
                    hour_4=predicts_hour4,hour_5=predicts_hour5,hour_6=predicts_hour6,hour_7=predicts_hour7,
                    hour_8=predicts_hour8,hour_9=predicts_hour9,hour_10=predicts_hour10,hour_11=predicts_hour11,
                    hour_12=predicts_hour12,hour_13=predicts_hour13,hour_14=predicts_hour14,hour_15=predicts_hour15,
                    hour_16=predicts_hour16,hour_17=predicts_hour17,hour_18=predicts_hour18,hour_19=predicts_hour19,
                    hour_20=predicts_hour20,hour_21=predicts_hour21,hour_22=predicts_hour22,hour_23=predicts_hour23)

lasso_pred=reshape(lasso_dt, 
        direction = "long",
        varying = list(names(lasso_dt)[2:25]),
        v.names = "consumption",
        idvar = c("Date"),
        timevar = "hour",
        times = 0:23)

lasso_pred=lasso_pred[order(date)]

perf_dt("First Logistic Regression(daily)", test_daily$Consumption, first_pred_daily)
perf_dt("Lasso Regression", test$consumption, lasso_pred$consumption)

library(rpart)
library(rattle)

reg_tree_daily=rpart(Consumption~.,train_daily,method='anova',minbucket=100)
fancyRpartPlot(reg_tree_daily)
reg_tree_daily$variable.importance

PredictCART_daily=predict(reg_tree_daily,newdata=test_daily)

perf_dt("Decision Tree-Daily",test_daily$Consumption,PredictCART_daily)

library(randomForest)

train_random_forest_daily=train_daily[8:.N]

random_forest_daily=randomForest(Consumption~.-date,data=train_random_forest_daily,
                           ntree=300,nodesize=30)
random_forest_daily

varImpPlot(random_forest_daily)

PredictRandomForest_daily=predict(random_forest_daily,newdata=test_daily)

perf_dt("Decision Tree-daily",test_daily$Consumption,PredictCART_daily)
perf_dt("Random Forest-daily",test_daily$Consumption,PredictRandomForest_daily)

library(caret)
library(e1071)

library(partykit)
library(psych)

set.seed(35)

numFolds=trainControl(method="cv",number = 10)
cpGrid=expand.grid(.cp=(0:50)*0.01)
tr=train(Consumption~.,
      data=train_random_forest_daily, 
      method="rpart",
      trControl=numFolds,
      tuneGrid= cpGrid)
tr

best_tree_daily=tr$finalModel
fancyRpartPlot(best_tree_daily)
best_tree_daily$variable.importance

# prediction_best_cv=predict(best_tree_daily,newdata=test_daily)

str(train_random_forest_daily)

lm_tree_daily=lmtree(Consumption~day_2+day_7+covid_severity+sunlight_time_minutes+reduced_temp|year(date)+month(date)+wday(date)+lock_down,data=train_random_forest_daily,
                    alpha=0.01)
lm_tree_daily
plot(lm_tree_daily)

test_daily$lock_down=as.numeric(test_daily$lock_down)-1

test_daily[lock_down==2,lock_down:=1]

test_daily$lock_down=as.factor(test_daily$lock_down)

str(test_daily)

perf_dt("Decision Tree-daily",test_daily$Consumption,PredictCART_daily)
perf_dt("Random Forest-daily",test_daily$Consumption,PredictRandomForest_daily)



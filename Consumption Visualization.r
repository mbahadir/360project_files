library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scrime)
library(lubridate)

install.packages("ggplot2")

dt=fread("bulk_consumption_with_temp.csv")

dt$month=month(dt$Date)
dt$week_day=wday(as.Date(dt$Date))

list<-c("Date", "Hour", "Consumption","week_day")

jan_obs=dt[month==1,colnames(dt) %in% list, with=FALSE]
feb_obs=dt[month==2,colnames(dt) %in% list, with=FALSE]

jan_obs %<>%  
  group_by(Date) %>% 
  mutate(norm_cons = (Consumption / sum(Consumption)))

ggplot(jan_obs,aes(x=Hour,y=Consumption,color=as.factor(Date)))+geom_line()+ theme(legend.position = "none")

ggplot(jan_obs,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "none")+geom_point()

feb_obs %<>%  
  group_by(Date) %>% 
  mutate(norm_cons = (Consumption / sum(Consumption)))

ggplot(feb_obs,aes(x=Hour,y=Consumption,color=Date))+geom_line()+ theme(legend.position = "none")

ggplot(feb_obs,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "none")+geom_point()

jan_week=filter(jan_obs,week_day!=1,week_day!=7)
jan_week_end=filter(jan_obs,week_day==1|week_day==7)

feb_week=filter(feb_obs,week_day!=1,week_day!=7)
feb_week_end=filter(feb_obs,week_day==1|week_day==7)

ggplot(jan_week,aes(x=Hour,y=norm_cons,color=as.factor(Date)))+geom_line()+ theme(legend.position = "none")+geom_point()

ggplot(jan_week_end,aes(x=Hour,y=norm_cons,color=as.factor(Date)))+geom_line()+ theme(legend.position = "none")+geom_point()

ggplot(feb_week,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "none")+geom_point()

ggplot(feb_week_end,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "none")+geom_point()

jan_week=filter(jan_obs,week_day!=1,week_day!=7)
jan_week_end=filter(jan_obs, (week_day==1|week_day==7))

jan_2017_week=filter(jan_obs,year(as.Date(Date))==2017, week_day!=1,week_day!=7)
jan_2018_week=filter(jan_obs,year(as.Date(Date))==2018, week_day!=1,week_day!=7)
jan_2019_week=filter(jan_obs,year(as.Date(Date))==2019, week_day!=1,week_day!=7)
jan_2020_week=filter(jan_obs,year(as.Date(Date))==2020, week_day!=1,week_day!=7)
feb_2017_week=filter(feb_obs,year(as.Date(Date))==2017, week_day!=1,week_day!=7)
feb_2018_week=filter(feb_obs,year(as.Date(Date))==2018, week_day!=1,week_day!=7)
feb_2019_week=filter(feb_obs,year(as.Date(Date))==2019, week_day!=1,week_day!=7)
feb_2020_week=filter(feb_obs,year(as.Date(Date))==2020, week_day!=1,week_day!=7)

jan_2017_week_end=filter(jan_obs,year(as.Date(Date))==2017, (week_day==1|week_day==7))
jan_2018_week_end=filter(jan_obs,year(as.Date(Date))==2018, (week_day==1|week_day==7))
jan_2019_week_end=filter(jan_obs,year(as.Date(Date))==2019, (week_day==1|week_day==7))
jan_2020_week_end=filter(jan_obs,year(as.Date(Date))==2020, (week_day==1|week_day==7))
feb_2017_week_end=filter(feb_obs,year(as.Date(Date))==2017, (week_day==1|week_day==7))
feb_2018_week_end=filter(feb_obs,year(as.Date(Date))==2018, (week_day==1|week_day==7))
feb_2019_week_end=filter(feb_obs,year(as.Date(Date))==2019, (week_day==1|week_day==7))
feb_2020_week_end=filter(feb_obs,year(as.Date(Date))==2020, (week_day==1|week_day==7))

ggplot(jan_2017_week,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "none")+geom_point()

ggplot(jan_2017_week_end,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "none")+geom_point()

ggplot(feb_2017_week,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "none")+geom_point()

ggplot(feb_2017_week_end,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "none")+geom_point()

ggplot(jan_2018_week,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "bottom")+geom_point()

ggplot(jan_2018_week_end,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "none")+geom_point()

ggplot(feb_2018_week,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "none")+geom_point()

ggplot(feb_2018_week_end,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "none")+geom_point()

ggplot(jan_2019_week,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "bottom")+geom_point()

ggplot(jan_2019_week_end,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "none")+geom_point()

ggplot(feb_2019_week,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "none")+geom_point()

ggplot(feb_2019_week_end,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "none")+geom_point()

ggplot(jan_2020_week,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "bottom")+geom_point()

ggplot(jan_2020_week_end,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "none")+geom_point()

ggplot(feb_2020_week,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "none")+geom_point()

ggplot(feb_2020_week_end,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "none")+geom_point()

jan_obs_sat=filter(jan_obs,week_day==7,yday(Date)!=1)
jan_obs_sun=filter(jan_obs,week_day==1,yday(Date)!=1)

feb_obs_sat=filter(feb_obs,week_day==7)
feb_obs_sun=filter(feb_obs,week_day==1)

ggplot(jan_obs_sat,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "bottom")+geom_point()

ggplot(jan_obs_sun,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "bottom")+geom_point()

head(jan_week)

jan_week_norm=filter(jan_week, week_day!=6,yday(Date)!=1)
jan_obs_fri=filter(jan_week, week_day==6,yday(Date)!=1)

feb_week_norm=filter(feb_week, week_day!=6)
feb_obs_fri=filter(feb_week, week_day==6)

ggplot(jan_week_norm,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "bottom")+geom_point()

ggplot(jan_obs_fri,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "bottom")+geom_point()

ggplot(jan_obs_sat,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "bottom")+geom_point()

ggplot(jan_obs_sun,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "bottom")+geom_point()

ggplot(feb_week_norm,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "none")+geom_point()

ggplot(feb_obs_fri,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "bottom")+geom_point()

feb_obs_sat=filter(feb_obs_sat,year(Date)!=2019)

ggplot(feb_obs_sat,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "bottom")+geom_point()

feb_obs_sun=filter(feb_obs_sun,year(Date)>=2019)

ggplot(feb_obs_sun,aes(x=Hour,y=norm_cons,color=Date))+geom_line()+ theme(legend.position = "bottom")+geom_point()

head(jan_week_norm)

jan_week_dt = jan_week_norm %>% 
  group_by(Hour) %>% 
  summarise(percentage = mean(norm_cons),
            week_day = unique(week_day)
           )

jan_fri_dt =jan_obs_fri %>% 
  group_by(Hour) %>% 
  summarise(percentage = mean(norm_cons),
            week_day = 6
           )

jan_sat_dt =jan_obs_sat %>% 
  group_by(Hour) %>% 
  summarise(percentage = mean(norm_cons),
            week_day = 7
           )

jan_sun_dt =jan_obs_sun %>% 
  group_by(Hour) %>% 
  summarise(percentage = mean(norm_cons),
            week_day = 1
           )

feb_week_dt =feb_week_norm %>% 
  group_by(Hour) %>% 
  summarise(percentage = mean(norm_cons),
            week_day = unique(week_day)
           )

feb_fri_dt =feb_obs_fri %>% 
  group_by(Hour) %>% 
  summarise(percentage = mean(norm_cons),
            week_day = 6
           )

feb_sat_dt =feb_obs_sat %>% 
  group_by(Hour) %>% 
  summarise(percentage = mean(norm_cons),
            week_day = 7
           )

feb_sun_dt =feb_obs_sun %>% 
  group_by(Hour) %>% 
  summarise(percentage = mean(norm_cons),
            week_day = 1
           )

library(plyr)

jan_dt=rbind.fill(jan_week_dt,jan_fri_dt,jan_sat_dt,jan_sun_dt)
feb_dt=rbind.fill(feb_week_dt,feb_fri_dt,feb_sat_dt,feb_sun_dt)

jan_dt=jan_dt[order(jan_dt$Hour),]
feb_dt=feb_dt[order(feb_dt$Hour),]

write.csv(jan_dt,'January_Consumption_Percentage.csv',row.names=FALSE)
write.csv(feb_dt,'February_Consumption_Percentage.csv',row.names=FALSE)

library(urca)
library(forecast)

jan_week_dt=as.data.table(jan_week_dt)
jan_week_norm=as.data.table(jan_week_norm)

jan_fri_dt=as.data.table(jan_fri_dt)
jan_obs_fri=as.data.table(jan_obs_fri)

jan_sat_dt=as.data.table(jan_sat_dt)
jan_obs_sat=as.data.table(jan_obs_sat)

jan_sun_dt=as.data.table(jan_sun_dt)
jan_obs_sun=as.data.table(jan_obs_sun)


feb_week_dt=as.data.table(feb_week_dt)
feb_week_norm=as.data.table(feb_week_norm)

feb_fri_dt=as.data.table(feb_fri_dt)
feb_obs_fri=as.data.table(feb_obs_fri)

feb_sat_dt=as.data.table(feb_sat_dt)
feb_obs_sat=as.data.table(feb_obs_sat)

feb_sun_dt=as.data.table(feb_sun_dt)
feb_obs_sun=as.data.table(feb_obs_sun)

jan_week_dt[Hour==0,]

plot(jan_obs_fri[Hour==12,]$norm_cons,type="l")

unt_test=ur.kpss(jan_obs_fri[Hour==12,]$norm_cons) 
summary(unt_test)





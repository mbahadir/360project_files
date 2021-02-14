library(data.table)
library(dplyr)

dt=fread("bulk_consumption_with_temp.csv")

dt_new=fread("all_features_df.csv")
droped=fread("droped_all_features_df.csv")

dt$Date=as.Date(dt$Date)
str(dt)

dt_new$date=as.Date(dt_new$date)
str(dt_new)

dt_daily =dt %>% 
  group_by(Date) %>% 
  summarise(Consumption = sum(Consumption),
            T_1=mean(T_1),
            T_2=mean(T_2),
            T_3=mean(T_3),
            T_4=mean(T_4),
            T_5=mean(T_5),
            T_6=mean(T_6),
            T_7=mean(T_7),
           )

cons_city=data.table(ant=7061598.02, adn=6032238.75, kon=4528707.86 , 
                     izm=15862142.35 , esk=2559348.73, ank=12144912.02 , 
                     ist=36926578.92 )

cons_city

write.csv(cons_city,"Eneryg_consumption_cities.csv",row.names=FALSE)

trans_temp=transmute(dt_daily,T_1=T_1*cons_city$ant,
                  T_2=T_2*cons_city$adn,
                  T_3=T_3*cons_city$kon,
                  T_4=T_4*cons_city$izm,
                  T_5=T_5*cons_city$esk,
                  T_6=T_6*cons_city$ank,
                  T_7=T_7*cons_city$ist)


form_pca1 <-prcomp(trans_temp, center = TRUE,scale. = TRUE)
form_pca1
summary(form_pca1)



form_pca <-prcomp(dt_daily[,c(3,4,5,6,7,8,9)], center = TRUE,scale. = TRUE)
form_pca
summary(form_pca)

dt_daily=mutate(dt_daily, reduced_temp= 0.3794502 * T_1 + 0.3748089 * T_2 + 0.3782977 * T_3 + 0.3800144 * T_4 + 0.3790882 * T_5 + 0.3803995 * T_6 + 0.3736363 * T_7)

tail(dt_daily)

write.csv(dt_daily,"Daily_values_with_reduced_temperature.csv",row.names=FALSE)



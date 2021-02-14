#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np
import seaborn as sns
import datetime as dt
import pytrends
from pytrends.request import TrendReq
from datetime import date, datetime, timedelta
import investpy


# # Update Covid Data

# In[2]:


full_lockdown = [dt.date(2020, 4, 11),
                 dt.date(2020, 4, 12),
                 dt.date(2020, 4, 18),
                 dt.date(2020, 4, 19),
                 dt.date(2020, 4, 23),
                 dt.date(2020, 4, 24),
                 dt.date(2020, 4, 25),
                 dt.date(2020, 4, 26),
                 
                 dt.date(2020, 5, 1),
                 dt.date(2020, 5, 2),
                 dt.date(2020, 5, 3),
                 dt.date(2020, 5, 9),
                 dt.date(2020, 5, 10),
                 dt.date(2020, 5, 16),
                 dt.date(2020, 5, 17),
                 dt.date(2020, 5, 18),
                 dt.date(2020, 5, 19),
                 dt.date(2020, 5, 23),
                 dt.date(2020, 5, 24),
                 dt.date(2020, 5, 25),
                 dt.date(2020, 5, 26),
                 dt.date(2020, 5, 30),
                 dt.date(2020, 5, 31),
                 
                 dt.date(2020, 12, 5),
                 dt.date(2020, 12, 6),
                 dt.date(2020, 12, 12),
                 dt.date(2020, 12, 13),
                 dt.date(2020, 12, 19),
                 dt.date(2020, 12, 20),
                 dt.date(2020, 12, 26),
                 dt.date(2020, 12, 27),
                         
                 dt.date(2021, 1, 1),
                 dt.date(2021, 1, 2),
                 dt.date(2021, 1, 3),
                 dt.date(2021, 1, 9),
                 dt.date(2021, 1, 10),
                 dt.date(2021, 1, 16),
                 dt.date(2021, 1, 17),
                 dt.date(2021, 1, 23),
                 dt.date(2021, 1, 24),
                 dt.date(2021, 1, 30),
                 dt.date(2021, 1, 31),
                 
                 dt.date(2021, 2, 6),
                 dt.date(2021, 2, 7),
                 dt.date(2021, 2, 11),
                 dt.date(2021, 2, 13),
                 dt.date(2021, 2, 14)]

partial_lockdown = [dt.date(2021, 12, 4),
                 dt.date(2021, 12, 7),
                 dt.date(2021, 12, 8),
                 dt.date(2021, 12, 9),
                 dt.date(2021, 12, 10),
                 dt.date(2021, 12, 11),
                 dt.date(2021, 12, 14),
                 dt.date(2021, 12, 15),
                 dt.date(2021, 12, 16),
                 dt.date(2021, 12, 17),
                 dt.date(2021, 12, 18),
                 dt.date(2021, 12, 21),
                 dt.date(2021, 12, 22),
                 dt.date(2021, 12, 23),
                 dt.date(2021, 12, 24),
                 dt.date(2021, 12, 25),
                 dt.date(2021, 12, 28),                
                 dt.date(2021, 12, 29),
                 dt.date(2021, 12, 30),
                 dt.date(2021, 12, 31),
                    
                 dt.date(2021, 1, 4),
                 dt.date(2021, 1, 5),
                 dt.date(2021, 1, 6),
                 dt.date(2021, 1, 7),
                 dt.date(2021, 1, 8),
                 dt.date(2021, 1, 11),
                 dt.date(2021, 1, 12),
                 dt.date(2021, 1, 13),
                 dt.date(2021, 1, 14),
                 dt.date(2021, 1, 15),
                 dt.date(2021, 1, 18),
                 dt.date(2021, 1, 19),
                 dt.date(2021, 1, 20),
                 dt.date(2021, 1, 21),
                 dt.date(2021, 1, 22),
                 dt.date(2021, 1, 25),
                 dt.date(2021, 1, 26),
                 dt.date(2021, 1, 27),
                 dt.date(2021, 1, 28),
                 dt.date(2021, 1, 29),

                 dt.date(2021, 2, 1),
                 dt.date(2021, 2, 2),
                 dt.date(2021, 2, 3),
                 dt.date(2021, 2, 4),
                 dt.date(2021, 2, 5),
                 dt.date(2021, 2, 8),
                 dt.date(2021, 2, 9),
                 dt.date(2021, 2, 10),
                 dt.date(2021, 2, 11),
                 dt.date(2021, 2, 12)]

today = dt.datetime.today()

# Specify the parameters to your liking
start_date= date(2019, 12, 1) # specify your start date
end_date= date(today.year, today.month, today.day) # specify your end date
key_word = '/g/11j2cc_qll' # use one key word
_cat = 0 # Category to narrow down your results
_geo = 'TR' # Two letter country abbreviation
_gprop = '' # What Google property to filter to (e.g 'images')
_hl = 'tr-TR' # Specify Language and Region
_tz = 180 # specify your time-zone

covid_data = pd.read_csv("Python Code\oguzhan_work\covid_and_lockdown.csv").sort_values(by="date")
covid_data.columns = ["date", "lock_down", "covid_severity"]
covid_data.date.apply(lambda x: dt.datetime.strptime(x, "%Y-%m-%d"))
covid_data = covid_data.set_index("date")
print(covid_data)

_timeframe = 'now 7-d'
totalTrend = TrendReq(hl=_hl, tz=_tz)
totalTrend.build_payload([key_word], cat=_cat, timeframe=_timeframe, geo=_geo, gprop=_gprop)
totalTrend = totalTrend.interest_over_time()
totalTrend.columns = ["covid_severity", "is_partial"]
totalTrend["date_no_hours"] = totalTrend.index
totalTrend.date_no_hours = totalTrend.date_no_hours.apply(lambda x: str(dt.date(x.year, x.month, x.day)))
last_week_covid = totalTrend.groupby("date_no_hours").mean()

last_available_severity = covid_data.iloc[-3, 1]
last_available_date = covid_data.index[-3]

normalizer = last_available_severity / last_week_covid.loc[last_available_date, "covid_severity"]
last_week_covid.covid_severity = last_week_covid.covid_severity*normalizer


covid_data.loc[last_week_covid.index, "covid_severity"] = last_week_covid.covid_severity
#unavailable_covid_data = last_week_covid[last_week_covid.index>last_available_date]
#
#print(unavailable_covid_data)
#
#if len(unavailable_covid_data):  
#    unavailable_covid_data.loc[:, "lock_down"] = 0
#    for i in unavailable_covid_data.index:
#        my_date = dt.datetime.strptime(i, "%Y-%m-%d")
#        my_date = dt.date(my_date.year, my_date.month, my_date.day)
#        
#        if my_date in partial_lockdown:
#            unavailable_covid_data.loc[i, "lock_down"] = 1
#        elif my_date in full_lockdown:
#            unavailable_covid_data.loc[i, "lock_down"] = 2
#    covid_data = covid_data.append(unavailable_covid_data)
#    
#try:
#    covid_data = covid_data.drop("is_partial", 1)
#except:
#    pass


covid_data.loc[str(dt.date.today() + dt.timedelta(days=1)), ] = (0 ,covid_data.iloc[-1, 1])

if dt.date.today() + dt.timedelta(days=1) in partial_lockdown:
    covid_data.loc[str(dt.date.today() + dt.timedelta(days=1)), "lock_down"] = 1
elif dt.date.today() + dt.timedelta(days=1) in full_lockdown:
    covid_data.loc[str(dt.date.today() + dt.timedelta(days=1)), "lock_down"] = 2

#covid_data.loc[covid_data.index.isin(last_week_covid.index), "covid_severity"] = last_week_covid.covid_severity
covid_data.loc["2021-02-03", :] = 1,30.775823
covid_data.to_csv(r"Python Code\oguzhan_work\covid_and_lockdown.csv")
covid_data


# # Update Holidays Data

# In[3]:


#official_holidays = pd.read_excel("Python Code\oguzhan_work\official holidays.xlsx")
#
#def date_converter(x):
#    x = x[:x.rfind(" " )]
#    x = x.replace(" ", "-")
#    x = x.replace("Ocak", "01")
#    x = x.replace("Şubat", "02")
#    x = x.replace("Mart", "03")
#    x = x.replace("Nisan", "04")
#    x = x.replace("Mayıs", "05")
#    x = x.replace("Haziran", "06")
#    x = x.replace("Temmuz", "07")
#    x = x.replace("Ağustos", "08")
#    x = x.replace("Eylül", "09")
#    x = x.replace("Ekim", "10")
#    x = x.replace("Kasım", "11")
#    x = x.replace("Aralık", "12")
#    x = dt.datetime.strptime(x, "%d-%m-%Y")
#    return x
#official_holidays.date = official_holidays.date.apply(lambda x: date_converter(x))
#
#nat_hols = ["Zafer Bayramı", "Ulusal Egemenlik ve Çocuk Bayramı", "Atatürk'ü Anma, Gençlik ve Spor Bayramı", "Cumhuriyet Bayramı"
#           , "Emek ve Dayanışma Günü*"]
#
#kurban = ["Kurban Bayramı (4.Gün)", "Kurban Bayramı (1.Gün)", "Kurban Bayramı (2.Gün)", "Kurban Bayramı (3.Gün)"]
#ramazan = ["Ramazan Bayramı (1.Gün)", "Ramazan Bayramı (2.Gün)","Ramazan Bayramı (3.Gün)"]
#
## Add yılbaşı, kurban, ramazan, 15 temmuz seperately
#my_date = dt.datetime(2017,1,1)
#holidays_data = pd.DataFrame(columns=["holiday_val"])
#
#
#while my_date <= dt.datetime.today() + dt.timedelta(days=1):
#    holidays_data.loc[my_date,"holiday_val"] = 0
#    tmp_holiday_data = official_holidays[official_holidays.date==my_date] 
#    
#    if len(tmp_holiday_data) > 0:
#        holidays_data.loc[my_date,"holiday_val"] = tmp_holiday_data.iloc[0,1]
#
#    my_date = my_date + dt.timedelta(days=1)
#    
##Yılbaşı: 1
##Milli bayram: 2 (15 July included)
##kurban: 3
##kurban arefesi:4
##ramazan: 5
##ramazan arefesi: 6
## birleşen tatil: 7
#
#holidays_data.loc[holidays_data.holiday_val=="Yılbaşı", "holiday_val"] = 1
#holidays_data.loc[holidays_data.holiday_val.isin(nat_hols), "holiday_val"] = 2
#holidays_data.loc[holidays_data.holiday_val.isin(kurban), "holiday_val"] = 3
#holidays_data.loc[holidays_data.holiday_val == "Kurban Bayramı Arefesi", "holiday_val"] = 4
#holidays_data.loc[holidays_data.holiday_val.isin(ramazan), "holiday_val"] = 5
#holidays_data.loc[holidays_data.holiday_val == "Ramazan Bayramı Arefesi", "holiday_val"] = 6
#holidays_data.loc[holidays_data.holiday_val =="Cumhuriyet Bayramı Arefesi", "holiday_val"] = 0
#
#for i in holidays_data.index:
#    if ((i.month==7) & (i.day==15)):
#        holidays_data.loc[i, "holiday_val"] = 2
#   
#    # Add weekday columns:
#    holidays_data.loc[i, "weekday"] = i.weekday()+1
#    
#    #Add tatil birleştirme feature:
#    if i==holidays_data.index[-1]:
#        next_day = i
#    else:
#        next_day = i + dt.timedelta(days=1)
#    
#    if i==dt.datetime(2017,1,1):
#        prv_day=i
#    else:
#        prv_day = i - dt.timedelta(days=1)
#    
#    if ((i.weekday()+1)==1) & (holidays_data.loc[i,"holiday_val"]==0) & (holidays_data.loc[next_day, "holiday_val"] != 0):
#        holidays_data.loc[i,"holiday_val"] = 7
#    
#    if ((i.weekday()+1)==5) & (holidays_data.loc[i,"holiday_val"]==0) & (holidays_data.loc[prv_day, "holiday_val"] != 0):
#        holidays_data.loc[i,"holiday_val"] = 7
#        
#holidays_data.to_csv(r"Python Code\oguzhan_work\holidays_data.csv")   
#holidays_data


# In[4]:


#new_hol_fac = pd.read_csv(r"Python Code\oguzhan_work\new_hol_fac.csv")
#new_hol_fac["date"] = new_hol_fac.date.apply(lambda x: dt.datetime.strptime(x, "%Y-%m-%d"))
#new_hol_fac["weekday"] = new_hol_fac.date.apply(lambda x: x.weekday()+1)
#new_hol_fac["month"] = new_hol_fac.date.apply(lambda x: x.month)
#
#new_hol_fac = new_hol_fac.drop("Unnamed: 0", 1).set_index("date")
#
#holidays_data =  new_hol_fac.copy()

# From this point it will only update the new date.


# In[5]:


holidays_data = pd.read_csv(r"Python Code\oguzhan_work\holidays_data.csv")
holidays_data["date"] = holidays_data.date.apply(lambda x: dt.datetime.strptime(x, "%Y-%m-%d"))
holidays_data = holidays_data.set_index("date")
my_date = holidays_data.index[-1]

while my_date <= dt.datetime.today() + dt.timedelta(days=1):
    holidays_data.loc[my_date,"holiday_val_factor"] = 0
    holidays_data.loc[my_date,"weekday"] = my_date.weekday() + 1
    holidays_data.loc[my_date,"month"] = my_date.month

    my_date = my_date + dt.timedelta(days=1)

holidays_data.to_csv(r"Python Code\oguzhan_work\holidays_data.csv")   
holidays_data


# In[6]:


holidays_data.holiday_val_factor.value_counts()


# # Add USD TRY data

# In[7]:


search_results = investpy.search_quotes(text='USD TRY',
                                        products=['currencies'],
                                        countries=['Turkey'],
                                        n_results=10)
str_today = str(dt.date.today())
str_today = str_today[-2:] + "/" + str_today[-5:-3] + "/" + str_today[:4]
str_today

usdtry = search_results[0].retrieve_historical_data(from_date='30/12/2016', to_date=str_today)
usdtry = usdtry.Close
usdtry


# # Update TCMB data

# In[8]:


erbis_new_economic = pd.read_excel(r"Python Code\oguzhan_work\EVDS (14).xlsx")[:49]
erbis_new_economic


# In[9]:


electricity_prices = pd.read_excel(r"Python Code\oguzhan_work\electricprices.xlsx")
electricity_prices.columns = ["date", "electricity_prices"] 

erbis_new_economic = pd.read_excel(r"Python Code\oguzhan_work\EVDS (14).xlsx")[:49]
erbis_new_economic.columns = ["date", "price_of_eletricity", "general_price_level", "production_capacity_rate"]
erbis_new_economic = erbis_new_economic.drop("price_of_eletricity", 1)#.to_csv("TCMB_Jan31_updated_data.csv")
erbis_new_economic = erbis_new_economic.merge(electricity_prices, how="right")
erbis_new_economic.date = erbis_new_economic.date.apply(lambda x: dt.datetime.strptime(x, "%Y-%m"))
erbis_new_economic = erbis_new_economic.set_index("date")
erbis_new_economic.loc[dt.datetime(2021,2,1), "production_capacity_rate"] = 75.5


# In[10]:


my_date = dt.datetime(2017,1,1)
tcmb_data = pd.DataFrame(columns=["production_capacity_rate", "price_of_electricity"])

my_date = dt.datetime(2017,1,1)
tcmb_data = pd.DataFrame(columns=["production_capacity_rate", "price_of_electricity"])

while my_date < dt.datetime.today() + dt.timedelta(days=1):
    tmp_df = erbis_new_economic[erbis_new_economic.index <= my_date] 
    
    tcmb_data.loc[my_date,"production_capacity_rate"] = tmp_df.iloc[-1, 1]
    tcmb_data.loc[my_date,"price_of_electricity"] = tmp_df.iloc[-1, 2]
    #tcmb_data.loc[my_date,"laborforce"] = tmp_df.iloc[-1, 2]
    #tcmb_data.loc[my_date,"unemployment"] = tmp_df.iloc[-1, 3]
    #tcmb_data.loc[my_date,"export"] = tmp_df.iloc[-1, 4]
    
    my_date = my_date + dt.timedelta(days=1)
    
tcmb_data["usdtry"] = usdtry

my_index = tcmb_data.loc[tcmb_data.usdtry.isna(), "usdtry"].index
tcmb_data.loc[tcmb_data.usdtry.isna(), "usdtry"] = usdtry[my_index - dt.timedelta(days=1)].values

my_index = tcmb_data.loc[tcmb_data.usdtry.isna(), "usdtry"].index
tcmb_data.loc[tcmb_data.usdtry.isna(), "usdtry"] = usdtry[my_index - dt.timedelta(days=2)].values

my_index = tcmb_data.loc[tcmb_data.usdtry.isna(), "usdtry"].index
tcmb_data.loc[tcmb_data.usdtry.isna(), "usdtry"] = usdtry[my_index - dt.timedelta(days=3)].values

tcmb_data.to_csv(r"Python Code\oguzhan_work\tcmb_data.csv")
tcmb_data


# In[11]:


#df = pd.read_excel(r"Python Code\oguzhan_work\EVDS (11).xlsx")[:151]
#df.Tarih = df.Tarih.apply(lambda x: dt.datetime.strptime(x, "%Y-%m"))
#
#new_economic_features = pd.read_csv(r"Python Code\oguzhan_work\new_economic_features.csv")
#new_economic_features["date"] = new_economic_features.apply(lambda x: dt.datetime(int(x.year), int(x.month), 1), axis=1)
#new_economic_features = new_economic_features[["date", "laborforce", "unemployment", "export"]].set_index("date")
#
#df = df.set_index("Tarih")
#df = df.merge(new_economic_features, how="right", right_index=True, left_index=True)
#df.loc[dt.datetime(2021,1,1), "TP FG J045"] = 551.5
#df.loc[dt.datetime(2021,1,1), "TP KKO2 IS TOP"] = 75.5
#
#my_date = dt.datetime(2017,1,1)
#tcmb_data = pd.DataFrame(columns=["production_capacity_rate", "price_of_electricity", "laborforce", "unemployment", "export"])
#
#while my_date < dt.datetime.today() + dt.timedelta(days=1):
#    tmp_df = df[df.index <= my_date] 
#    
#    tcmb_data.loc[my_date,"production_capacity_rate"] = tmp_df.iloc[-1, 1]
#    tcmb_data.loc[my_date,"price_of_electricity"] = tmp_df.iloc[-1, 0]
#    tcmb_data.loc[my_date,"laborforce"] = tmp_df.iloc[-1, 2]
#    tcmb_data.loc[my_date,"unemployment"] = tmp_df.iloc[-1, 3]
#    tcmb_data.loc[my_date,"export"] = tmp_df.iloc[-1, 4]
#    
#    my_date = my_date + dt.timedelta(days=1)
#    
#tcmb_data.to_csv(r"Python Code\oguzhan_work\tcmb_data.csv")
#tcmb_data


# In[ ]:





# In[ ]:





# # Update sunlight time data

# In[12]:


prayer_times_17 = pd.read_csv(r"Python Code\oguzhan_work\prayertimes\2017_prayer_times.csv")
prayer_times_18 = pd.read_csv(r"Python Code\oguzhan_work\prayertimes\2018_prayer_times.csv")
prayer_times_19 = pd.read_csv(r"Python Code\oguzhan_work\prayertimes\2019_prayer_times.csv")
prayer_times_20 = pd.read_csv(r"Python Code\oguzhan_work\prayertimes\2020_prayer_times.csv")
prayer_times_21 = pd.read_csv(r"Python Code\oguzhan_work\prayertimes\2021_prayer_times.csv")

prayer_times_list = [prayer_times_17, prayer_times_18, prayer_times_19, prayer_times_20, prayer_times_21]
prayer_times_df = pd.concat(prayer_times_list, axis=0)
prayer_times_df["date.readable"] = prayer_times_df["date.readable"].apply(lambda x: dt.datetime.strptime(x, "%d %b %Y"))

prayer_times_df = prayer_times_df[['timings.Fajr', 'timings.Sunrise', 'timings.Dhuhr', 'timings.Asr',
       'timings.Sunset', 'timings.Maghrib', 'timings.Isha', 'timings.Imsak',
       'timings.Midnight', 'date.readable']]

prayer_times_df.columns = ['Fajr', 'Sunrise', 'Dhuhr', 'Asr',
       'Sunset', 'Maghrib', 'Isha', 'Imsak',
       'Midnight', 'date']

prayer_times_df = prayer_times_df.set_index("date")

for i in prayer_times_df.columns:
    prayer_times_df.loc[:,i] = prayer_times_df[i].apply(lambda x: x.split()[0], 0)

prayer_times_df["sunlight_time_minutes"] = prayer_times_df.Sunset.apply(lambda x: dt.datetime.strptime(x, "%H:%M")) - prayer_times_df.Sunrise.apply(lambda x: dt.datetime.strptime(x, "%H:%M"))
prayer_times_df.sunlight_time_minutes = prayer_times_df.sunlight_time_minutes.apply(lambda x: round(x.seconds/60),0)

prayer_times_df["date"] = prayer_times_df.index
prayer_times_df.Sunrise = prayer_times_df.apply(lambda x: dt.datetime.strptime(str(x.date)[:-8] + (str(x.Sunrise)), "%Y-%m-%d %H:%M"), axis=1)
prayer_times_df.Sunset = prayer_times_df.apply(lambda x: dt.datetime.strptime(str(x.date)[:-8] + (str(x.Sunset)), "%Y-%m-%d %H:%M"), axis=1)
prayer_times_df = prayer_times_df.drop("date",1 )

prayer_times_df.to_csv(r"Python Code\oguzhan_work\prayer_times_df.csv")
prayer_times_df


# 
# # Merge the data

# In[13]:


covid =    pd.read_csv(r"Python Code\oguzhan_work\covid_and_lockdown.csv")
holidays = pd.read_csv(r"Python Code\oguzhan_work\holidays_data.csv")
new_hols = pd.read_csv(r"Python Code\oguzhan_work\new_holiday_data_frame.csv")

#holidays.holiday_val = new_hols.holiday_val
#holidays = holidays.fillna(0)


feature_df = holidays.merge(covid, how="outer", left_on="date", right_on="date")
feature_df.columns = ["date", 'holiday_val(as_factor)', 'weekday(as_factor)', "month_as_factor", 'lock_down(as_factor)', 'covid_severity']
feature_df = feature_df.set_index("date")
prayer_times = pd.read_csv(r"Python Code\oguzhan_work\prayer_times_df.csv")

tcmb_data = pd.read_csv(r"Python Code\oguzhan_work\tcmb_data.csv")
tcmb_data = tcmb_data.set_index("Unnamed: 0")

feature_df = feature_df.merge(prayer_times[["date", "sunlight_time_minutes"]], how="left", left_index=True, right_on="date").set_index("date")
feature_df = feature_df.merge(tcmb_data, how="left", left_index=True, right_index=True)

feature_df.columns = ['holiday_val_factor', 'weekday_factor', "month_factor", 'lock_down_factor',
       'covid_severity', 'sunlight_time_minutes', 'production_capacity_rate',
       'price_of_electricity', 'usdtry']

#feature_df = feature_df.drop("unemployment", 1)

feature_df.to_csv("all_features_df.csv")
feature_df


# # Create features with dummy  

# In[14]:


holidays


# In[15]:


holiday_val = pd.get_dummies(feature_df["holiday_val_factor"]).iloc[:,1:]
holiday_val.columns = ["new_year", "nat_holiday", "sacrifice_holiday", "sacrifice_eve", "ramadan_holiday", "ramadan_eve",  "monday_or_friday_between_holidays", "extra_holidays", "extra_holidays_2"]

week_days = pd.get_dummies(feature_df["weekday_factor"])
week_days.columns = ["monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"]

months = pd.get_dummies(feature_df["month_factor"])
months.columns = ["Jan", "Feb", "Mar", "Apr", "May", "June", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]


lock_downs = pd.get_dummies(feature_df["lock_down_factor"]).iloc[:,1:]
lock_downs.columns = ["partial_lockdown", "full_lockdown"]


# In[16]:


all_features_with_dummies = feature_df.merge(week_days, how="left", right_index= True, left_index=True).merge(months, how="left", right_index= True, left_index=True).merge(holiday_val, how="left", right_index= True, left_index=True).merge(lock_downs, how="left", right_index= True, left_index=True)
all_features_with_dummies = all_features_with_dummies.drop(["holiday_val_factor", "weekday_factor", "lock_down_factor","month_factor"], 1)
all_features_with_dummies.to_csv("all_features_with_dummies.csv")
all_features_with_dummies


# In[17]:


all_features_with_dummies.T


# In[18]:


all_features_with_dummies.describe().T


# In[19]:


start_date = dt.datetime(2017,1,1,0,0)
dates_list = []
my_today = dt.datetime.today()

while start_date < dt.datetime(my_today.year, my_today.month, my_today.day, 23,59) + dt.timedelta(days=1):
    dates_list.append(start_date) 
    start_date = start_date + dt.timedelta(hours=1)
start_date


# In[20]:


hourly_df = pd.DataFrame({"date" : dates_list})
hourly_df["is_lockdown"] = 0 
hourly_df["date_by_day"] = hourly_df.date.apply(lambda x: str(x)[:10])

#all_features_df = all_features_df.set_index("date")
for i in feature_df.index:
    if feature_df.loc[i,"lock_down_factor"] == 2:
        hourly_df.loc[hourly_df.date_by_day==i, "is_lockdown"] = 1
        
    elif feature_df.loc[i,"lock_down_factor"] == 1:
        hourly_df.loc[(hourly_df.date>=dt.datetime(int(i[:4]), int(i[5:7]), int(i[8:10]), 21,0)) & (hourly_df.date < dt.datetime(int(i[:4]), int(i[5:7]), int(i[8:10]), 21,0) + dt.timedelta(hours=8)), "is_lockdown"] = 1
        #print( hourly_df.loc[(hourly_df.date>dt.datetime(int(i[:4]), int(i[5:7]), int(i[8:10]), 21,0)) & (hourly_df.date < dt.datetime(int(i[:4]), int(i[5:7]), int(i[8:10]), 21,0) + dt.timedelta(hours=8)), "is_lockdown"])
        

    
hourly_df


# # Create Hourly Feature df

# In[21]:


start_date = dt.datetime(2017,1,1,0,0)
dates_list = []
my_today = dt.datetime.today()

while start_date < dt.datetime(my_today.year, my_today.month, my_today.day, 23,59) + dt.timedelta(days=1):
    dates_list.append(start_date) 
    start_date = start_date + dt.timedelta(hours=1)
start_date


# In[22]:


hourly_df = pd.DataFrame({"date" : dates_list})
hourly_df["is_lockdown"] = 0 
hourly_df["date_by_day"] = hourly_df.date.apply(lambda x: str(x)[:10])
hourly_df["is_sun"] = 0

#all_features_df = all_features_df.set_index("date")
for i in feature_df.index:
    if feature_df.loc[i,"lock_down_factor"] == 2:
        hourly_df.loc[hourly_df.date_by_day==i, "is_lockdown"] = 1
        
    elif feature_df.loc[i,"lock_down_factor"] == 1:
        hourly_df.loc[(hourly_df.date>=dt.datetime(int(i[:4]), int(i[5:7]), int(i[8:10]), 21,0)) & (hourly_df.date < dt.datetime(int(i[:4]), int(i[5:7]), int(i[8:10]), 21,0) + dt.timedelta(hours=8)), "is_lockdown"] = 1
        #print( hourly_df.loc[(hourly_df.date>dt.datetime(int(i[:4]), int(i[5:7]), int(i[8:10]), 21,0)) & (hourly_df.date < dt.datetime(int(i[:4]), int(i[5:7]), int(i[8:10]), 21,0) + dt.timedelta(hours=8)), "is_lockdown"])
        
    py_times = prayer_times_df.loc[dt.date(int(i[:4]), int(i[5:7]), int(i[8:10])), :]
    hourly_df.loc[(hourly_df.date > py_times.Sunrise) & (hourly_df.date < py_times.Sunset), "is_sun"] = 1
    
    
    hourly_df.loc[hourly_df.date_by_day==i, "holiday_val_factor"]       = feature_df.loc[i, "holiday_val_factor"]
    hourly_df.loc[hourly_df.date_by_day==i, "weekday_factor"]           = feature_df.loc[i, "weekday_factor"]
    hourly_df.loc[hourly_df.date_by_day==i, "covid_severity"]           = feature_df.loc[i, "covid_severity"]
    hourly_df.loc[hourly_df.date_by_day==i, "production_capacity_rate"] = feature_df.loc[i, "production_capacity_rate"]
    hourly_df.loc[hourly_df.date_by_day==i, "price_of_electricity"]     = feature_df.loc[i, "price_of_electricity"]
    hourly_df.loc[hourly_df.date_by_day==i, "usdtry"]                   = feature_df.loc[i, "usdtry"]
    
hourly_df =  hourly_df.drop("date_by_day",1)
hourly_df


# In[23]:


hourly_df.describe()


# In[24]:


hourly_df.to_csv("hourly_feature_df.csv")


# In[ ]:





# In[25]:


#get_data_until


# In[26]:


#hourly_feature_df = pd.DataFrame(columns = feature_df.columns)
#hourly_feature_df["is_sun"] = 0
#
#my_datetime = dt.datetime(2017,1,1,0,0)
#tomorrow = dt.datetime.today() + dt.timedelta(days=1)
#get_data_until = dt.datetime(tomorrow.year, tomorrow.month, tomorrow.day, 23,59)
#
#columns_wont_change = feature_df.columns
#mt=0
#while my_datetime < get_data_until:
#    hourly_feature_df.loc[my_datetime,columns_wont_change]  = feature_df.loc[str(dt.date(my_datetime.year, my_datetime.month, my_datetime.day)), columns_wont_change]
#    if (hourly_feature_df.loc[my_datetime,'lock_down_factor'] == 1) & ((my_datetime.hour<21) & (my_datetime.hour>5)):
#        hourly_feature_df.loc[my_datetime,'lock_down_factor'] = 0
#        
#    # change sunlight time 
#    hourly_feature_df[my_datetime, "is_sun"] = 0
#    py_times = prayer_times_df.loc[dt.date(my_datetime.year, my_datetime.month, my_datetime.day), :]
#
#    if (my_datetime > py_times.Sunrise) & (my_datetime < py_times.Sunset):
#        hourly_feature_df[my_datetime, "is_sun"] = 1 
#    
#    my_datetime = my_datetime + timedelta(hours=1)
#    
#    if my_datetime.month !=mt:
#        mt = my_datetime.month
#        print(mt)
#hourly_feature_df.lock_down_factor = hourly_feature_df.lock_down_factor.replace(2,1)


# In[27]:


#hourly_feature_df


# In[28]:


#holiday_val = pd.get_dummies(hourly_feature_df["holiday_val_factor"]).iloc[:,1:]
#holiday_val.columns = ["new_year", "nat_holiday", "sacrifice_holiday", "sacrifice_eve", "ramadan_holiday", "ramadan_eve",  "monday_or_friday_between_holidays", "extra_holidays"]
#
#week_days = pd.get_dummies(hourly_feature_df["weekday_factor"])
#week_days.columns = ["monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"]


# In[ ]:





# In[29]:


#hourly_feature_df.to_csv


# In[ ]:





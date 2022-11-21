library(tidyverse)
library(dplyr)
library(readxl)

setwd("..")
home <- getwd()
data_out <- paste0(home,"/Data")
setwd("./Training_access")

# data path for local data
data_path = "B:/Randomization/2022-09"

#load data
file_used=c("20220912_Datenlieferung_1-3.csv")
wave_2_raw=paste(data_path,file_used, sep = "/") %>% 
  read.csv(., sep=";" , dec = ",")

#check with pilot people
#load our data from first pilot
file_used = c("sample_phase1_penr.csv")
pilot_penr = paste("B:",file_used, sep = "/") %>% 
  read.csv(., sep=";" , dec = ",")

file_used = c("penr_list.xlsx")
penrlist = paste("B:",file_used, sep = "/") %>% 
  read_excel()

double<-inner_join(wave_2_raw,pilot_penr,by="penr")
double1<-inner_join(wave_2_raw,penrlist,by=c("penr"="PENR"))
test<-anti_join(double,double1,by="penr")
#no matter which list, the same 205 need to be kicked out
wave_2_raw<-anti_join(wave_2_raw,double, by="penr")

file_used = c("wave_1_assigned.xlsx")
wave1_penr = paste("B:/Randomization/2022-07/",file_used, sep = "/") %>% 
  read_excel()%>%select(penr)

double<-inner_join(wave_2_raw,wave1_penr,by="penr")
#noone from first wave

rm(double, double1, pilot_penr,penrlist,wave1_penr)

# aggregate variables for stratified randomisation
wave_2 =
  wave_2_raw  %>%
  mutate(
    penr = as.integer(penr),
    region=case_when(rgs==301 |rgs==316 | rgs==317|rgs==326|rgs==328|rgs==3310|rgs==333~"Mo",
                     rgs==311 | rgs==313|rgs==315|rgs==332|rgs==335~"Wa",
                     rgs==3080 | rgs==312|rgs==314|rgs==319~"We",
                     rgs==304 | rgs==306|rgs==321|rgs==323|rgs==329|rgs==334~"In"),
    nationality_AUT = as.integer(nation == "A"),
    male = as.integer(geschl == "M"),
    agegr= case_when(alter<35~"y",
                     alter>34 & alter<50 ~ "m",
                     alter>49 ~ "o"),
    marginal_employment = ifelse(geringf=="GER",1,0),
    education = case_when(ausb%in%c("AK","FB","FH","UB","UV","HA","HB","HK","HS","HT")~1,TRUE~0),
    German_ok = case_when(deutschk %in% c("K","A","A1","A2","B1","B2","B")~0,TRUE~1),# more than B
    unemp_dur = case_when(gf=="3_GF3Q"~"3Q",
                          gf=="4_GF4Q"~"4Q",
                          gf=="5_GF1J"~"1J")
    )

#Korrekturen
wave_2$education[wave_2$ausb=="XX"]<-NA 
wave_2$nationality_AUT[wave_2$nation=="X"]<-NA 

wave_2=wave_2%>%select(penr,
                nationality_AUT, 
                male,
                agegr,
                region,
                marginal_employment, 
                education, 
                education, 
                German_ok,
                unemp_dur)%>%filter(!is.na(education))
#lose 3 observations here

#import isco and recode
library(readxl)
file_used=c("isco-help.xlsx")
isco<-paste(home,"/helpfiles/", file_used, sep = "/") %>% 
  read_excel( col_types = c("numeric", "numeric", "text") )

isco$beruf<-isco$BERUF_6
isco$BERUF_6<-NULL

temp<-left_join(wave_2_raw,isco,by=c("beruf"),copy=TRUE)
temp$penr = as.integer(temp$penr)
temp<-temp%>%select(penr,beruf,ISCO08_1)

wave_2<-left_join(wave_2,temp,by="penr")
summary(wave_2$ISCO08_1)

rm(temp,isco)
#write

wave_2 %>% 
  readr::write_csv(paste(data_path,
                  "wave_2.csv",
                  sep = "/"))

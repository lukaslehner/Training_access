library(tidyverse)
library(dplyr)
library(readxl)

setwd("..")
home <- getwd()
data_out <- paste0(home,"/Data-rep")
setwd("./Training_access")

# data path for local data
data_path = "A:/Training_access_2022_23/Randomization/July22"

#load data
#everybody GF3Q,GF4Q,GF1J
file_used=c("20220711_Datenlieferung_1-2.csv")
wave_1_raw=paste(data_path,file_used, sep = "/") %>% 
  read.csv(., sep=";" , dec = ",")

#check with pilot people
#load our data from first pilot
file_used = c("full_data.xlsx")
pilot_penr = paste("A:",file_used, sep = "/") %>% 
  read_excel()%>%select(personal_id)

double<-inner_join(wave_1_raw,pilot_penr,by=c("penr"="personal_id"))
wave_1_raw<-anti_join(wave_1_raw,double, by="penr")
rm(double, pilot_penr)

# aggregate variables for stratified randomisation
wave_1 =
  wave_1_raw  %>%
  mutate(
    personal_id = as.integer(penr),
    region=case_when(rgs==301 |rgs==316 | rgs==317|rgs==326|rgs==328|rgs==3310|rgs==333~"Mo",
                     rgs==311 | rgs==313|rgs==315|rgs==332|rgs==335~"Wa",
                     rgs==3080 | rgs==312|rgs==314|rgs==319~"We",
                     rgs==304 | rgs==306|rgs==321|rgs==323|rgs==329|rgs==334~"In"),
    nationality_AUT = as.integer(nation == "A"),
    male = as.integer(geschl == "M"),
    agegr= case_when(alter<35~"y",
                     alter>34 & alter<50 ~ "m",
                     alter>49 ~ "o"),
    marginal_employment = ifelse(is.na(geringf),0,1),
    education = case_when(ausb%in%c("AK","FB","FH","UB","UV","HA","HB","HK","HS","HT")~1,TRUE~0),
    German_ok = case_when(deutschk %in% c("K","A","A1","A2","B1","B2","B")~0,TRUE~1),# more than B
    unemp_dur = case_when(gf=="3_GF3Q"~"3Q",
                          gf=="4_GF4Q"~"4Q",
                          gf=="5_GF1J"~"1J")
    )

#Korrekturen
wave_1$education[wave_1$ausb=="XX"]<-NA 
wave_1$nationality_AUT[wave_1$nation=="X"]<-NA 

wave_1=wave_1%>%select(personal_id,
                nationality_AUT, 
                male,
                agegr,
                region,
                marginal_employment, 
                education, 
                education, 
                German_ok,
                unemp_dur)%>%filter(!is.na(education)&!is.na(nationality_AUT))
#lose 25 observations here

#import isco and recode
library(readxl)
file_used=c("isco-help.xlsx")
isco<-paste("A:", file_used, sep = "/") %>% 
  read_excel( col_types = c("numeric", "numeric", "text") )

isco$beruf<-isco$BERUF_6
isco$BERUF_6<-NULL

temp<-left_join(wave_1_raw,isco,by=c("beruf"),copy=TRUE)
temp$personal_id = as.integer(temp$penr)
temp<-temp%>%select(personal_id,beruf,ISCO08_1)

wave_1<-left_join(wave_1,temp,by=c("personal_id"))
summary(wave_1$ISCO08_1)

rm(temp,isco)
#write

wave_1 %>% 
  readr::write_csv(paste(data_path,
                  "wave_1.csv",
                  sep = "/"))

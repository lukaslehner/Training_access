library(tidyverse)
library(dplyr)
library(readxl)

setwd("..")
home <- getwd()
data_out <- paste0(home,"/Data")
setwd("./Training_access")

data_path="B:/Randomization"

#load waves

dates<-c("2022-07","2022-09","2022-11","2023-01","2023-03","2023-05","2023-07")
fulldata_3<-NA

for(i in 1:7){
  if(i==1){
    wave1=paste(data_path,"2022-07/wave_1_assigned.xlsx",sep="/")%>%
      read_excel()
    wave1$wave<-i
    fulldata_3<-rbind(fulldata_3,wave1)
  }else{
    data<-paste0(data_path,"/",dates[i],"/wave_",i,"_assigned.xlsx")%>%read_excel()
    data$error<-0
    data$wave<-i
    fulldata_3<-rbind(fulldata_3,data)
  }
}
rm(data)
fulldata_3<-fulldata_3[-1,]

write_xlsx(fulldata_3,"B:/full_data_3.xlsx")




###### Stratified Randomization for treatment assignment
library(randomizr)
library(readr)

setwd("..")
home <- getwd()
data_out <- paste0(home,"/Data")
setwd("./Training_access")
# data path for local data
data_path = "B:/Randomization/2023-07"

### 1. Main covariate file and merge ---------------------------------

# reading in participant file from source data
file_used = c("wave_7.csv")
wave_7 = paste(data_path, file_used, sep = "/") %>% 
  read_delim(delim = ",", locale = locale(encoding = "latin1", decimal_mark = ","))

### Stratification
#variables have to be defined as factors for stratification
wave_7$educ_f<-as.factor(wave_7$education)
wave_7$agegr_f<-as.factor(wave_7$agegr)
wave_7$male_f<-as.factor(wave_7$male)
wave_7$region_f<-as.factor(wave_7$region)
wave_7$unemp_durf<-as.factor(wave_7$unemp_dur)

wave_7$strata<-interaction(wave_7[,c("educ_f","agegr_f","male_f","unemp_durf")])
summary(wave_7$strata)

#------------------------------------------
set.seed(4320)
wave_7$group_nr<-block_ra(blocks=wave_7$strata,prob_each = rep((1/3),3),num_arms=3)

#check
library(expss)
library(xtable)
library(ggplot2)

#strata sizes plot
#no observations per strata
wave_7<-wave_7 %>% 
  group_by(strata) %>%
  mutate(no_rows = length(penr))%>%ungroup()

wave_7$strata = with(wave_7, reorder(strata, no_rows, median))
wave_7 %>% filter(!is.na(strata))%>%
  ggplot( aes(x=strata))+geom_bar()+labs(x="",y="observations",title = "Strata sizes")+
  theme_minimal()+theme(axis.text.x=element_blank())
ggsave("strataplot_7.png", path=data_out)

#z-test between column percents each compared with each
table11<-wave_7%>%tab_cells(educ_f,agegr_f,region_f,male_f,as.factor(nationality_AUT),
                            as.factor(marginal_employment),as.factor(German_ok),unemp_durf)%>%
  tab_cols(group_nr)%>%tab_stat_cpct()%>%tab_last_sig_cpct()%>%tab_pivot(stat_position = "outside_rows")
table11

print(xtable(table11,digits=1,include.colnames=FALSE,caption = "Treatment Balance"),  include.rownames=FALSE, caption.placement = 'top')

#Chi-squared test of difference between the groups
library(arsenal)
tab1 <- tableby(group_nr ~ male_f + agegr_f+ educ_f+region_f+as.factor(nationality_AUT)+unemp_durf+
                as.factor(marginal_employment)+as.factor(German_ok), data=wave_7)
summary(tab1)
setwd(data_out)

capture.output(summary(tab1), file="Test_w7.md")

## Convert R Markdown Table to LaTeX
require(knitr)
require(rmarkdown)
render("Test_w7.md", pdf_document(keep_tex=TRUE))
setwd("..")

# exporting files for PES; one for each group of newsletters
library("writexl")

control<-wave_7%>%ungroup()%>%filter(group_nr=="T1")%>%select(penr)
Vleduc<-wave_7%>%ungroup()%>%filter(group_nr=="T2" & educ_f==0)%>%select(penr)
Vheduc<-wave_7%>%ungroup()%>%filter(group_nr=="T2" & educ_f==1)%>%select(penr)
Vinfleduc<-wave_7%>%ungroup()%>%filter(group_nr=="T3" & educ_f==0)%>%select(penr)
Vinfheduc<-wave_7%>%ungroup()%>%filter(group_nr=="T3" & educ_f==1)%>%select(penr)

write_xlsx(control,paste(data_path,"wave_7_Control.xlsx", sep="/"))
write_xlsx(Vleduc,paste(data_path,"wave_7_Voucher_loweduc.xlsx", sep="/"))
write_xlsx(Vheduc,paste(data_path,"wave_7_Voucher_higheduc.xlsx", sep="/"))
write_xlsx(Vinfleduc,paste(data_path,"wave_7_Voucherinfo_loweduc.xlsx", sep="/"))
write_xlsx(Vinfheduc,paste(data_path,"wave_7_Voucherinfo_higheduc.xlsx", sep="/"))

wave_7%>%select(-c(beruf,education,agegr,male,region,unemp_dur))%>%
  write_xlsx(., paste(data_path,"wave_7_assigned.xlsx",sep="/"))

rm(list=ls())

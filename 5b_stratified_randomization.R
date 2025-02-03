###### Stratified Randomization for treatment assignment
library(randomizr)
library(readr)

setwd("..")
home <- getwd()
data_out <- paste0(home,"/Data")
setwd("./Training_access")
# data path for local data
data_path = "B:/Randomization/2023-03"

### 1. Main covariate file and merge ---------------------------------

# reading in participant file from source data
file_used = c("wave_5.csv")
wave_5 = paste(data_path, file_used, sep = "/") %>% 
  read_delim(delim = ",", locale = locale(encoding = "latin1", decimal_mark = ","))

### Stratification
#variables have to be defined as factors for stratification
wave_5$educ_f<-as.factor(wave_5$education)
wave_5$agegr_f<-as.factor(wave_5$agegr)
wave_5$male_f<-as.factor(wave_5$male)
wave_5$region_f<-as.factor(wave_5$region)
wave_5$unemp_durf<-as.factor(wave_5$unemp_dur)

wave_5$strata<-interaction(wave_5[,c("educ_f","agegr_f","male_f","unemp_durf")])
summary(wave_5$strata)

#------------------------------------------
set.seed(1111)
wave_5$group_nr<-block_ra(blocks=wave_5$strata,prob_each = rep((1/3),3),num_arms=3)

#check
library(expss)
library(xtable)
library(ggplot2)

#strata sizes plot
#no observations per strata
wave_5<-wave_5 %>% 
  group_by(strata) %>%
  mutate(no_rows = length(penr))%>%ungroup()

wave_5$strata = with(wave_5, reorder(strata, no_rows, median))
wave_5 %>% filter(!is.na(strata))%>%
  ggplot( aes(x=strata))+geom_bar()+labs(x="",y="observations",title = "Strata sizes")+
  theme_minimal()+theme(axis.text.x=element_blank())
ggsave("strataplot_5.png", path=data_out)

#z-test between column percents each compared with each
table11<-wave_5%>%tab_cells(educ_f,agegr_f,region_f,male_f,as.factor(nationality_AUT),
                            as.factor(marginal_employment),as.factor(German_ok),unemp_durf)%>%
  tab_cols(group_nr)%>%tab_stat_cpct()%>%tab_last_sig_cpct()%>%tab_pivot(stat_position = "outside_rows")
table11

print(xtable(table11,digits=1,include.colnames=FALSE,caption = "Treatment Balance"),  include.rownames=FALSE, caption.placement = 'top')

#Chi-squared test of difference between the groups
library(arsenal)
tab1 <- tableby(group_nr ~ male_f + agegr_f+ educ_f+region_f+as.factor(nationality_AUT)+unemp_durf+
                as.factor(marginal_employment)+as.factor(German_ok), data=wave_5)
summary(tab1)
setwd(data_out)

capture.output(summary(tab1), file="Test_w5.md")

## Convert R Markdown Table to LaTeX
require(knitr)
require(rmarkdown)
render("Test_w5.md", pdf_document(keep_tex=TRUE))
setwd("..")

# exporting files for PES; one for each group of newsletters
library("writexl")

control<-wave_5%>%ungroup()%>%filter(group_nr=="T1")%>%select(penr)
Vleduc<-wave_5%>%ungroup()%>%filter(group_nr=="T2" & educ_f==0)%>%select(penr)
Vheduc<-wave_5%>%ungroup()%>%filter(group_nr=="T2" & educ_f==1)%>%select(penr)
Vinfleduc<-wave_5%>%ungroup()%>%filter(group_nr=="T3" & educ_f==0)%>%select(penr)
Vinfheduc<-wave_5%>%ungroup()%>%filter(group_nr=="T3" & educ_f==1)%>%select(penr)

write_xlsx(control,paste(data_path,"wave_5_Control.xlsx", sep="/"))
write_xlsx(Vleduc,paste(data_path,"wave_5_Voucher_loweduc.xlsx", sep="/"))
write_xlsx(Vheduc,paste(data_path,"wave_5_Voucher_higheduc.xlsx", sep="/"))
write_xlsx(Vinfleduc,paste(data_path,"wave_5_Voucherinfo_loweduc.xlsx", sep="/"))
write_xlsx(Vinfheduc,paste(data_path,"wave_5_Voucherinfo_higheduc.xlsx", sep="/"))

wave_5%>%select(-c(beruf,no_rows,education,agegr,male,region,unemp_dur))%>%
  write_xlsx(., paste(data_path,"wave_5_assigned.xlsx",sep="/"))

rm(list=ls())

###### Stratified Randomization for treatment assignment
library(randomizr)
library(readr)

### 1. Main covariate file and merge ---------------------------------

# reading in participant file from source data
file_used = c("wave_1.csv")
wave_1 = paste(data_path, file_used, sep = "/") %>% 
  read_delim(delim = ",", locale = locale(encoding = "latin1", decimal_mark = ","))

### Stratification
#variables have to be defined as factors for stratification
wave_1$educ_f<-as.factor(wave_1$education)
wave_1$agegr_f<-as.factor(wave_1$agegr)
wave_1$male_f<-as.factor(wave_1$male)
wave_1$region_f<-as.factor(wave_1$region)
wave_1$unemp_durf<-as.factor(wave_1$unemp_dur)

wave_1$strata<-interaction(wave_1[,c("educ_f","agegr_f","male_f","unemp_durf")])
summary(wave_1$strata)

#------------------------------------------
set.seed(3003)
wave_1$group_nr<-block_ra(blocks=wave_1$strata,prob_each = rep((1/3),3),num_arms=3)

#check
library(expss)
library(xtable)
library(ggplot2)

#strata sizes plot
#no observations per strata
wave_1<-wave_1 %>% 
  group_by(strata) %>%
  mutate(no_rows = length(personal_id))%>%ungroup()

wave_1$strata = with(wave_1, reorder(strata, no_rows, median))
wave_1 %>% filter(!is.na(strata))%>%
  ggplot( aes(x=strata))+geom_bar()+labs(x="",y="observations",title = "Strata sizes")+
  theme_minimal()+theme(axis.text.x=element_blank())
ggsave("strataplot_1.png", path=data_out)

#z-test between column percents each compared with each
table11<-wave_1%>%tab_cells(educ_f,agegr_f,region_f,male_f,as.factor(nationality_AUT),
                            as.factor(marginal_employment),as.factor(German_ok),unemp_durf)%>%
  tab_cols(group_nr)%>%tab_stat_cpct()%>%tab_last_sig_cpct()%>%tab_pivot(stat_position = "outside_rows")

print(xtable(table11,digits=1,include.colnames=FALSE,caption = "Treatment Balance"),  include.rownames=FALSE, caption.placement = 'top')

#Chi-squared test of difference between the groups
library(arsenal)
tab1 <- tableby(group_nr ~ male_f + agegr_f+ educ_f+region_f+as.factor(nationality_AUT)+unemp_durf+
                as.factor(marginal_employment)+as.factor(German_ok), data=wave_1)
summary(tab1)
setwd(data_out)

setwd(data_out)
capture.output(summary(tab1), file="Test_w1.md")

## Convert R Markdown Table to LaTeX
require(knitr)
require(rmarkdown)
render("Test_w1.md", pdf_document(keep_tex=FALSE))
setwd("..")

# exporting files for PES; one for each group of newsletters
library("writexl")

control<-wave_1%>%ungroup()%>%filter(group_nr=="T1")%>%select(personal_id)
Vleduc<-wave_1%>%ungroup()%>%filter(group_nr=="T2" & educ_f==0)%>%select(personal_id)
Vheduc<-wave_1%>%ungroup()%>%filter(group_nr=="T2" & educ_f==1)%>%select(personal_id)
Vinfleduc<-wave_1%>%ungroup()%>%filter(group_nr=="T3" & educ_f==0)%>%select(personal_id)
Vinfheduc<-wave_1%>%ungroup()%>%filter(group_nr=="T3" & educ_f==1)%>%select(personal_id)

write_xlsx(control,paste(data_path,"wave_1_Control.xlsx", sep="/"))
write_xlsx(Vleduc,paste(data_path,"wave_1_Voucher_loweduc.xlsx", sep="/"))
write_xlsx(Vheduc,paste(data_path,"wave_1_Voucher_higheduc.xlsx", sep="/"))
write_xlsx(Vinfleduc,paste(data_path,"wave_1_Voucherinfo_loweduc.xlsx", sep="/"))
write_xlsx(Vinfheduc,paste(data_path,"wave_1_Voucherinfo_higheduc.xlsx", sep="/"))

wave_1%>%select(-c(beruf,no_rows,education,agegr,male,region,unemp_dur))%>%
  write_xlsx(., paste(data_path,"wave_1_assigned.xlsx",sep="/"))

rm(list=ls())

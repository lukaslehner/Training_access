library(tidyverse)
library(lubridate)

setwd("..")
home <- getwd()
data_out <- paste0(home,"/Data-rep")
setwd("./Training_access")

# data path for local data
# data_path = "A:/Wiederholung22-23/Randomisierung/Juli22"
data_path = "A:/Training_access_2022_23/Randomization/July22"


# 1- Wave 1: July-22 Stratified randomization  -------- 

# Data preparation for randomization
source("1a_randomization_data_prep.R")

# Constructing the treatment assignment
source("1b_stratified_randomization.R")




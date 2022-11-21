library(tidyverse)
library(lubridate)

setwd("..")
home <- getwd()
data_out <- paste0(home,"/Data-rep")
setwd("./Training_access")

# 1- Wave 1: July-22 Stratified randomization  -------- 

# Data preparation for randomization
source("1a_randomization_data_prep.R")

# Constructing the treatment assignment
source("1b_stratified_randomization.R")

# 2- Wave 2: Sep-22 Stratified randomization  -------- 

# Data preparation for randomization
source("2a_randomization_data_prep.R")

# Constructing the treatment assignment
source("2b_stratified_randomization.R")

# 3- Wave 3: Nov-22 Stratified randomization  -------- 

# Data preparation for randomization
source("3a_randomization_data_prep.R")

# Constructing the treatment assignment
source("3b_stratified_randomization.R")


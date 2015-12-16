library(survey)
library(foreign)
library(data.table)
library(dplyr)
library(magrittr)
library(tidyr)

hilda_master <- 
  read.dta("../HILDA/Master_m130c.dta") %>%
  data.table
  
hilda <- read.dta("../HILDA/Household_j130c.dta")
hilda <- data.table(hilda)

hilda %>%
  select(jhwsupei, # Superannuation 
         jhwfini,
         jhwassei,
         jhgage1)
hilda[,1:5,with=F]

## Number of newstart allowance entrants as a
## result of transfers

library(readxl)
library(data.table)
library(dplyr)
library(magrittr)

GST_by_year <-
  read_excel("GST_by_year.xlsx", sheet = "GST Table 1", skip = 2) %>%
  setnames(1:2, c("Selected_item", "unit")) %>% 
  # en dash caution
  setnames(old = names(.), new = gsub("([0-9]{4}).([0-9]{2})", "\\1-\\2", names(.))) %>%
  gather(year, value, `2001-02`:`2013-14`) %>%
  mutate(value = as.numeric(value)) %>%
  filter(complete.cases(.)) %>%
  filter(grepl("Net.GST", Selected_item)) %>%
  mutate(value = value * 1e6,
         unit = "$")  # dollar units

EXTRA.TRANSFER.AMT <- 
  GST_by_year[GST_by_year$year == "2012-13", ]$value * 0.5 *  # 15 percent
  0.3  # amount going to transfers

EXTANT.TRANSFERS.201213 <- 131.6 * 10^9

NEWSTART.ALLOWANCE.RATIO <- 
  (EXTANT.TRANSFERS.201213 + EXTRA.TRANSFER.AMT) / EXTANT.TRANSFERS.201213

# Assume newstart will increase by this ratio.
allowance <- 
  12807.60  # http://www.cpaaustralia.com.au/~/media/corporate/allfiles/document/professional-resources/financial-planning/tax-social-security-guide-2012-13.pdf

allowance_with_income <- 
  function(fortnightly.income){
    fortnightly.income + max(0, 492.6 - (0.5 * (fortnightly.income - 62) * IND(fortnightly.income, min = 62, max = 250) +
                                           IND(fortnightly.income, min = 250) * 0.6 * (fortnightly.income - 250)))
  }

for (i in 492:2000){
  if (allowance_with_income(i) == i){
    allowance_cutoff_fortnightly <- i
    break
  }
}

ALLOWANCE.CUTOFF <- 
  allowance_cutoff_fortnightly * 26

tax201213 <- get_sample_file()

entrants <- 
  tax201213 %>%
  age_mutate_() %>%
  filter(Tot_inc_amt >= ALLOWANCE.CUTOFF, 
         Tot_inc_amt <= ALLOWANCE.CUTOFF * NEWSTART.ALLOWANCE.RATIO,
         Aust_govt_pnsn_allw_amt < 1,
         !(Age %in% c("under\n20", "65\nto\n69", "70\nand\nover")))

extants <- 
  tax201213 %>%
  age_mutate_() %>%
  filter(Tot_inc_amt <= ALLOWANCE.CUTOFF, 
         Aust_govt_pnsn_allw_amt > 0,
         !(Age %in% c("under\n20", "65\nto\n69", "70\nand\nover")))

# Prop increase in entrants
nrow(entrants) / nrow(extants)
# 25%

# This underestimates extants...
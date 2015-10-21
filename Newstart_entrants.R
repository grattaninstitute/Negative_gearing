## Number of newstart allowance entrants as a
## result of transfers

library(readxl)
library(data.table)
library(dplyr)
library(magrittr)

purl("GST_costing.Rmd")
source("GST_costing.R", echo=FALSE)
file.remove("GST_costing.R")

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
## How many individuals of a certian income are on newstart
# ====
sih11 %>% 
  mutate(Approximate_income = round(52 * Total_current_weekly_employee_income_as_reported, -3)) %>%
  group_by(Approximate_income) %>%
  summarise(on_newstart = mean(Current_weekly_income_from_newstart_allowance > 0)
            , N =n()) %>%
  grplot(aes(x = Approximate_income,
             y = on_newstart)) + 
  geom_point(aes(size = N)) + 
  stat_smooth() + 
  xlim(0,50e3) + 
  coord_cartesian(xlim = c(0,50e3)) + 
  geom_vline(xintercept = c(ALLOWANCE.CUTOFF, ALLOWANCE.CUTOFF * NEWSTART.ALLOWANCE.RATIO))

get_sample_file() %$%
  sum(Taxable_Income >= ALLOWANCE.CUTOFF & 
        Taxable_Income <= ALLOWANCE.CUTOFF * NEWSTART.ALLOWANCE.RATIO) * 52 / 1e6 * 0.05  


hes10_indiv %>%
  mutate(Approximate_income = round(52 * Total_current_weekly_income_from_all_sources, -3)) %>%
  group_by(Approximate_income) %>%
  summarise(on_newstart = weighted.mean(Current_weekly_income_from_newstart_allowance > 0,
                                        Weight_Person_HES)
            , N = sum(Weight_Person_HES)) %>%
  grplot(aes(x = Approximate_income,
             y = on_newstart)) + 
  geom_point(aes(size = N)) + 
  stat_smooth(aes(weight = N)) + 
  xlim(0, 50e3) + 
  coord_cartesian(xlim = c(0, 50e3)) + 
  geom_vline(xintercept = c(ALLOWANCE.CUTOFF, ALLOWANCE.CUTOFF * NEWSTART.ALLOWANCE.RATIO))

sih11 %>%
  mutate(Approximate_income = round(52 * Total_current_weekly_income_from_all_sources, -3)) %>%
  group_by(Approximate_income) %>%
  summarise(on_newstart = weighted.mean(Current_weekly_income_from_newstart_allowance > 0,
                                        Person_weight)
            , N = sum(Person_weight)) %>%
  grplot(aes(x = Approximate_income,
             y = on_newstart)) + 
  geom_point(aes(size = N)) + 
  stat_smooth(aes(weight = N)) + 
  xlim(0, 50e3) + 
  coord_cartesian(xlim = c(0, 50e3)) + 
  geom_vline(xintercept = c(ALLOWANCE.CUTOFF, ALLOWANCE.CUTOFF * NEWSTART.ALLOWANCE.RATIO))

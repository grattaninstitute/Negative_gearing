## 

purl("GST_costing.Rmd")
source("GST_costing.R", echo=FALSE)
file.remove("GST_costing.R")

hes10hh %>%
  mutate(disposable_income = Total_current_weekly_income_from_all_sources - Household_weekly_expenditure_on_income_tax_HES_only)

disposable.income.quintiles <- 
  svydesign(data = hes10hh, ids = ~Person_number_within_each_income_unit, weights = ~Person_weight) %>%
  svyquantile(~disposable_income, design = ., quantiles = (0:5)/5)

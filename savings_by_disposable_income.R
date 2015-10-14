## 

purl("GST_costing.Rmd")
source("GST_costing.R", echo=FALSE)
file.remove("GST_costing.R")

sih11 %<>%
  mutate(disposable_income = Total_current_weekly_income_from_all_sources - Imputed_current_weekly_tax_payable_basis)

disposable.income.quintiles <- 
  svydesign(data = sih11, ids = ~Person_number_within_each_income_unit, weights = ~Person_weight) %>%
  svyquantile(~disposable_income, design = ., quantiles = (0:5)/5)
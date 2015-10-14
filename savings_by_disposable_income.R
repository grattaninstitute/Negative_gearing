## 

purl("GST_costing.Rmd")
source("GST_costing.R", echo=FALSE)
file.remove("GST_costing.R")

hes10hh.orig <- hes10hh

hes10hh %<>%
  mutate(disposable_income = Total_current_weekly_HH_income_from_all_sources - Household_weekly_expenditure_on_income_tax_HES_only)

disposable.income.quintiles <- 
  svydesign(data = hes10hh, ids = ~Unique_household_number, weights = ~Weight_HH_HES) %>%
  svyquantile(~disposable_income, design = ., quantiles = (0:5)/5)

create_qiles <- function(x, breaks){
  as.numeric(factor(cut(x, breaks = breaks, include.lowest=TRUE)))
}

savings.by.quintile <- 
  hes10hh %>%
  mutate(
    quintiles = create_qiles(disposable_income, disposable.income.quintiles) 
  ) %>%
  mutate(savings = disposable_income - Total_goods_and_services_expenditure_HES_only) %>%
  group_by(quintiles) %>%
  summarise(avg_savings = weighted.mean(savings, Weight_HH_HES))

savings.by.quintile %>%
  grplot(aes(x = quintiles, y = avg_savings)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous("Weekly household savings", label=grattan_dollar) + 
  theme(axis.title.y = element_text(angle = 90))
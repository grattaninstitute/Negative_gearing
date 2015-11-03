# setwd("..")

# Functions present in other scripts
require(grattan)
require(data.table)
require(dplyr) && require(magrittr)

tax201213 <- get_sample_file()

tax201415 <- 
  tax201213 %>%
  mutate(Sw_amt = Sw_amt * wage_inflator(from_fy = "2012-13", to_fy = "2013-14")^2,
         Rptbl_Empr_spr_cont_amt = Rptbl_Empr_spr_cont_amt * wage_inflator(from_fy = "2012-13", to_fy = "2013-14")^2,
         Non_emp_spr_amt = Non_emp_spr_amt * wage_inflator(from_fy = "2012-13", to_fy = "2013-14")^2,
         #
         Taxable_Income = Taxable_Income * wage_inflator(from_fy = "2012-13", to_fy = "2013-14")^2)

old_tax <- 
  tax201415 %>%
  mutate(income_tax = income_tax(Taxable_Income, fy.year = "2017-18"),
         tax_on_contributions = ifelse(Taxable_Income > 300e3, 0.30, 0.15) * (Rptbl_Empr_spr_cont_amt + Non_emp_spr_amt + pmin(50e3, 0.095 * Sw_amt))) %$%
         {(sum(income_tax) + sum(tax_on_contributions)) * 50 * lf_inflator(to_date = "2015-06-30")}

new_tax <-
  tax201415 %>%
  mutate(income_tax = income_tax(Taxable_Income, fy.year = "2017-18"),
         tax_on_contributions = ifelse(Taxable_Income > 115e3, 0.30, 0.15) * (Rptbl_Empr_spr_cont_amt + Non_emp_spr_amt + pmin(50e3, 0.095 * Sw_amt))) %$%
         {(sum(income_tax) + sum(tax_on_contributions)) * 50 * lf_inflator(to_date = "2015-06-30")}

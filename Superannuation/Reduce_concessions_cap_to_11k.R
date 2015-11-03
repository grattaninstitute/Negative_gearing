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
         
revenue_from_cap <- function(cap = 30000, div293 = TRUE, div293.threshold = 300e3, super.guarantee.rate = 0.095){
  .tax201415 <- 
    tax201415 %>%
    mutate(prev_cap = 30e3) %>%
    mutate(super_income_in_excess_of_cap = pmax(0, Rptbl_Empr_spr_cont_amt + Non_emp_spr_amt + super.guarantee.rate * Sw_amt - prev_cap),
           
           new_concession = pmin(cap, Rptbl_Empr_spr_cont_amt + Non_emp_spr_amt + super.guarantee.rate * Sw_amt),
           new_super_income_in_excess_of_cap = pmax(0, Rptbl_Empr_spr_cont_amt + Non_emp_spr_amt + super.guarantee.rate * Sw_amt - cap),
             
           new_Taxable_Income = Taxable_Income - super_income_in_excess_of_cap + new_super_income_in_excess_of_cap)
  
  revenue.from.ordinary.income <- 
    .tax201415 %$%
    sum(income_tax(new_Taxable_Income) - income_tax(Taxable_Income)) * 50 * lf_inflator(to_date = "2015-06-30")
  
  if(div293){
    revenue.from.super.taxes <- 
      .tax201415 %$%
      sum(ifelse(new_Taxable_Income < div293.threshold, new_concession * 0.15, new_concession * 0.30)) * 50 * lf_inflator(to_date = "2015-06-30")
  } else {
    if(!missing(div293.threshold))
      warning("div293.threshold is provided, but div293 = FALSE")
    
    revenue.from.super.taxes <- 
      .tax201415 %$%
      sum(new_concession * 0.15) * 50 * lf_inflator(to_date = "2015-06-30")
  }
  .tax201415 <<- .tax201415  # for debugging
  return(revenue.from.ordinary.income)
}
# This function calculates the replacement rate of an individual. The
# **replacement rate** of an individual is 
#
#   (retirement income) / (lifetime income)
#
# So someone with an average (inflation-adjusted) income of $100,000/yr
# with a retirement income of $50,000/yr would have a replacement rate 
# of 50%.

library(data.table)
library(dplyr)

replacement_rate <- function(income_initial, 
                             age_initial = 32,
                             year_initial = 2015,
                             retirement_age = 67,
                             SG_policy = "2014-15 Budget",  # Legislated to rise to 12%
                             CPI = 0.025,
                             real_wage_growth = 0.015,
                             super_rate_of_return = 0.07,
                             earnings_tax_effective = 0.115,
                             contributions_tax = 0.15,
                             annuity_rate = 0.04,
                             annuity_period = 25
){
  if(SG_policy == "2014-15 Budget"){
    Tbl <- data.table(
      Age = seq(age_initial, retirement_age, by = 1L)
    ) %>%
      mutate(Year = (year_initial - 1L) + 1:n(),
             SG_rate = ifelse(Year <= 2020, 
                              0.095, 
                              pmin(0.12, 
                                   0.095 + 0.005 * (Year - 2020))),
             Salary = income_initial * (1 + real_wage_growth + CPI) ^ (Year - year_initial),
             contributions = Salary * SG_rate * (1 - contributions_tax))
    
    Tbl %<>% mutate(Super_balance = 0) 
    for (i in seq_along(seq(age_initial, retirement_age, by = 1L))){
      Tbl %<>%
        mutate(Super_balance = ifelse(Year == year_initial,
                                      contributions,
                                      
                                      lag(Super_balance) + lag(Super_balance) * super_rate_of_return * (1 - earnings_tax_effective) + contributions))
    }
    
    Tbl %<>% mutate(Super_balance_real = Super_balance / (1 + CPI + real_wage_growth) ^ (Year - year_initial))
    
  }
  
  Tbl %$%
    last(Super_balance_real) / ((1 - 1/(1 + annuity_rate)^annuity_period)/annuity_rate)
}
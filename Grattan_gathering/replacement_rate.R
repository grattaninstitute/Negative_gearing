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
                             year_initial = 2014,
                             retirement_age = 67,
                             termination_age = retirement_age + annuity_period,
                             SG_policy = "2014-15 Budget",  # Legislated to rise to 12%
                             CPI = 0.025,
                             real_wage_growth = 0.015,
                             super_rate_of_return = 0.07,
                             earnings_tax_effective = 0.115,
                             contributions_tax = 0.15,
                             annuity_rate = 0.04,
                             annuity_period = 25
){
    Accumulation <- data.table(
      Age = seq(age_initial, retirement_age + annuity_period, by = 1L),
      Super_balance = 0
    ) %>%
      mutate(Year = (year_initial - 1L) + 1:n(),
             sg_policy = SG_policy,  # for vector recycling
             SG_rate = ifelse(sg_policy == "2014-15 Budget",
                              ifelse(Year <= 2020, 
                                     0.095, 
                                     pmin(0.12, 0.095 + 0.005 * (Year - 2020))),
                              ifelse(SG_policy == "9.5%",
                                     0.095, NA_real_)),
             Salary = income_initial * (1 + real_wage_growth + CPI) ^ (Year - year_initial),
             contributions = Salary * SG_rate * (1 - contributions_tax),
             Retd = Age > retirement_age)
    
    ## Determine super balance
    balance_now <- function(prev_bal, contribution){
      prev_bal + prev_bal * super_rate_of_return * (1 - earnings_tax_effective) + contribution
    }
    
    Accumulation$Super_balance <- Reduce(f = balance_now, 
                                         x = Accumulation$contributions, 
                                         accumulate = TRUE)
    
    Accumulation[ ,Super_balance_real := Super_balance / (1 + CPI + real_wage_growth) ^ (Year - year_initial)]
    
    # Use wolfram alpha to solve ther difference equation
    # (which I could once do but WA is better)
    br <- function(initial_balance, 
                   n_years, 
                   annuity_initial, 
                   rate # both annuity and earnings
    ){
      ifelse(n_years == 0,
             initial_balance,
             (1 + rate)^(n_years) * (initial_balance - (1 + rate) * annuity_initial * n_years))
    }
    
    
    
    balance_retirement <- function(...){
      pmax(0, br(...))
    }
    
    final_accumulated_balance <- 
      Accumulation %>%
      dplyr::filter(Age == retirement_age) %$%
      Super_balance
    
    annuity_initial <- 
      uniroot(f = function(x) br(final_accumulated_balance, 
                                 n_years = termination_age - retirement_age,
                                 x,
                                 rate = annuity_rate),
              interval = c(1, final_accumulated_balance)) %$%
      root
    
    Retirement <- 
      data.table(Age = seq(retirement_age + 1, termination_age)) %>%
      mutate(Retirement_balance = balance_retirement(initial_balance = final_accumulated_balance,
                                                     n_years = Age - retirement_age,
                                                     annuity_initial = annuity_initial,
                                                     rate = annuity_rate),
             Annuity = annuity_initial * (1 + annuity_rate)^(1:n() - 1)
      )
  .debug <- merge(Accumulation, Retirement, by = "Age", all.x = TRUE, all.y = TRUE)

  last_salary <- 
    merge(Accumulation, Retirement, by = "Age", all.x = TRUE, all.y = TRUE) %>%
    mutate(Balance = ifelse(Retd, Retirement_balance, Super_balance)) %>%
    select(-Super_balance, -Retirement_balance) %>%
    filter(Retd, !lag(Retd)) %$%
    {Salary}
  
  first_annuity <- 
    merge(Accumulation, Retirement, by = "Age", all.x = TRUE, all.y = TRUE) %>%
    mutate(Balance = ifelse(Retd, Retirement_balance, Super_balance)) %>%
    select(-Super_balance, -Retirement_balance) %>%
    filter(Retd, !lag(Retd)) %$%
    {Annuity}
  
  age_pension <- function(income = first_annuity * (1 + CPI)^(age_initial - retirement_age),
                          assets = 0){
    fortnightly <- 788.40 - ifelse(income/26 < 162, 
                                   0, 
                                   ifelse(income/26 > 1896, 
                                          788.40,
                                          income/26 * 0.5))
    fortnightly * 26
  }
  
  .debug <<-
    .debug %>%
    mutate(age_pension = age_pension(income = Annuity/(1 + CPI)^(Age - age_initial)))
  
  (first_annuity + age_pension()) / income_initial
}


# setwd("..")

# Functions present in other scripts
require(grattan)
require(data.table)
require(dplyr) && require(magrittr)

tax201213 <- readr::read_csv("../IndividualSampleFile/2012-13/Sample_file_1213/2013 2% individuals sample file/2013_sample_file.csv")

tax201415 <- 
  tax201213 %>%
  mutate(Sw_amt = Sw_amt * wage_inflator(from_fy = "2012-13", to_fy = "2013-14")^2,
         Rptbl_Empr_spr_cont_amt = Rptbl_Empr_spr_cont_amt * wage_inflator(from_fy = "2012-13", to_fy = "2013-14")^2,
         Non_emp_spr_amt = Non_emp_spr_amt * wage_inflator(from_fy = "2012-13", to_fy = "2013-14")^2,
         #
         Taxable_Income = Taxable_Income * wage_inflator(from_fy = "2012-13", to_fy = "2013-14")^2)

# ====         
revenue_from_cap <- function(cap = 30000, cap2 = 35000, age_based_cap = FALSE, div293 = TRUE, div293.threshold = 300e3, super.guarantee.rate = 0.095){
  .tax201415 <- 
    tax201415 %>%
    # 50 year olds and over get a cap of 35e3
    mutate(prev_cap = ifelse(age_range <= 4, 35e3, 30e3)) %>%
    mutate(super_income_in_excess_of_cap = pmax(0, Rptbl_Empr_spr_cont_amt + Non_emp_spr_amt + super.guarantee.rate * Sw_amt - prev_cap),
           prv_concess_contr_amt = pmin(prev_cap, Rptbl_Empr_spr_cont_amt + Non_emp_spr_amt + super.guarantee.rate * Sw_amt),
           new_concess_contr_amt = pmin(ifelse(age_range <= 4, ifelse(age_based_cap, cap2, cap), cap), Rptbl_Empr_spr_cont_amt + Non_emp_spr_amt + super.guarantee.rate * Sw_amt),
           new_super_income_in_excess_of_cap = pmax(0, Rptbl_Empr_spr_cont_amt + Non_emp_spr_amt + super.guarantee.rate * Sw_amt - ifelse(age_range <= 4, ifelse(age_based_cap, cap2, cap), cap)),
             
           new_Taxable_Income = Taxable_Income - super_income_in_excess_of_cap + new_super_income_in_excess_of_cap,
           #
           revenue_due_to_Taxable_Income = income_tax(new_Taxable_Income, fy.year = "2015-16", include.temp.budget.repair.levy = FALSE) - income_tax(Taxable_Income, fy.year = "2015-16", include.temp.budget.repair.levy = FALSE))
  
  if(div293){
      .tax201415 %<>%
      mutate(
        prv_tax_payable_on_concess_contr = 
          ifelse(new_Taxable_Income + prv_concess_contr_amt < div293.threshold,
                 new_concess_contr_amt * 0.15, 
                 # This is probably not quite right yet.
                 ifelse(new_Taxable_Income < div293.threshold, 
                        0.15 * (div293.threshold - Taxable_Income) + 0.30 * (Taxable_Income + prv_concess_contr_amt - div293.threshold),
                        new_concess_contr_amt * 0.30)),
        
        new_tax_payable_on_concess_contr = 
          ifelse(new_Taxable_Income + prv_concess_contr_amt < div293.threshold,
                 new_concess_contr_amt * 0.15, 
                 # This is probably not quite right yet.
                 ifelse(new_Taxable_Income < div293.threshold, 
                        0.15 * (div293.threshold - new_Taxable_Income) + 0.30 * (new_Taxable_Income + new_concess_contr_amt - div293.threshold),
                        new_concess_contr_amt * 0.30))) 
  } else {
    if(!missing(div293.threshold))
      warning("div293.threshold is provided, but div293 = FALSE")
    
    .tax201415 %<>%
      mutate(prv_tax_payable_on_concess_contr = 0.15 * prv_concess_contr_amt,
             new_tax_payable_on_concess_contr = 0.15 * new_concess_contr_amt)
  }
    
  .tax201415 <- 
    .tax201415 %>%
    tbl_df %>%
    mutate(
      diff = revenue_due_to_Taxable_Income + new_tax_payable_on_concess_contr - prv_tax_payable_on_concess_contr,
      tax = income_tax(new_Taxable_Income, fy.year = "2015-16", include.temp.budget.repair.levy = FALSE) + new_tax_payable_on_concess_contr,
      debug_inner = new_Taxable_Income + new_concess_contr_amt,  # not at issue
      debug0 = income_tax(as.integer(debug_inner), fy.year = "2015-16", include.temp.budget.repair.levy = FALSE),
      debug = income_tax(new_Taxable_Income + new_concess_contr_amt,
                         fy.year = "2015-16", 
                         include.temp.budget.repair.levy = FALSE),
      
      extra_tax_on_contr_if_no_concession = income_tax(new_Taxable_Income + new_concess_contr_amt,
                                                       fy.year = "2015-16", 
                                                       include.temp.budget.repair.levy = FALSE) - 
        income_tax(new_Taxable_Income, 
                   fy.year = "2015-16", 
                   include.temp.budget.repair.levy = FALSE) - 
        new_tax_payable_on_concess_contr,
      
      tax_on_concession = new_tax_payable_on_concess_contr
    )
  # for debugging
  .tax201415 <<- .tax201415
  revenue <- sum(.tax201415$diff) * 50 * lf_inflator(to_date = "2015-06-30")
  return(revenue)
}

# ====
revenue_from_cap()
status_quo_tax_by_percentile <-
  .tax201415 %>%
  group_by(taxable_income_percentile = ntile(Taxable_Income, 100)) %>%
  summarise(mean_tax = mean(extra_tax_on_contr_if_no_concession))

rm(.tax201415)
gc(1,1)

revenue_from_cap(cap = 11000)
cap11k_tax_by_percentile <- 
  .tax201415 %>%
  group_by(taxable_income_percentile = ntile(Taxable_Income, 100)) %>%
  summarise(mean_tax = mean(extra_tax_on_contr_if_no_concession))

bind_rows(status_quo_tax_by_percentile, cap11k_tax_by_percentile, .id = "id") %>% 
  grplot(aes(x = taxable_income_percentile, y = mean_tax, color = id)) + 
  geom_line(aes(group = id)) + theme(legend.position = c(0.2,0.7))

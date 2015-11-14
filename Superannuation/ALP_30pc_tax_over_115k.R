
revenue_from_30pc_tax_over_115k <- function(cap = 30e3, cap2 = 35e3, age.based.cap = TRUE, super.guarantee.rate = 0.095){
  revenue_from_cap(age_based_cap = TRUE)
  nocap.taxstats <- .tax201415
  
  prev_tax_on_contr <- sum(nocap.taxstats$tax_on_concession) * 50
  
  rm(.tax201415)
  .tax201415.alp <-
  tax201415 %>% 
    mutate(
      prev_cap = ifelse(age_range <= 4, cap2, cap),
      super_contributions = pmin(prev_cap, 
                                 Rptbl_Empr_spr_cont_amt + Non_emp_spr_amt + super.guarantee.rate * Sw_amt),
      super_contributions_in_excess_of_cap = pmax(0, 
                                                  Rptbl_Empr_spr_cont_amt + Non_emp_spr_amt + super.guarantee.rate * Sw_amt - prev_cap),
      tax = .income_tax(Taxable_Income + super_contributions_in_excess_of_cap),
      tax_on_contr = ifelse(Taxable_Income > 115e3,
                            0.30,
                            0.15) * super_contributions,
      extra_tax_on_contr_if_no_concession = .income_tax(Taxable_Income + super_contributions_in_excess_of_cap + super_contributions) - .income_tax(Taxable_Income + super_contributions_in_excess_of_cap) - tax_on_contr
           )
  
  ALP_30pc_over_115k <<- .tax201415.alp
  revenue.difference <- (sum(ALP_30pc_over_115k$tax_on_contr) * 50 - prev_tax_on_contr) * lf_inflator(to_date = "2015-06-30")
  return(revenue.difference)
}
    
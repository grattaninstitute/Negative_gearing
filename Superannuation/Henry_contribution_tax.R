
.income_tax <- function(Taxable_Income){
  income_tax(Taxable_Income, fy.year = "2015-16", include.temp.budget.repair.levy = FALSE)
}

henry_coster <- function(marginal_rate_subtractor = 0.20, super.guarantee.rate = 0.095, div293 = TRUE){
  .tax201415 <- 
  tax201415 %>%
    mutate(
      super_pretax_contributions = Rptbl_Empr_spr_cont_amt + Non_emp_spr_amt + super.guarantee.rate * Sw_amt,
      previous_tax = .income_tax(Taxable_Income),
      new_tax = .income_tax(Taxable_Income + super_pretax_contributions),
      rebate = super_pretax_contributions * marginal_rate_subtractor
    ) %>%
    mutate(
      prev_cap = ifelse(age_range <= 4, 35e3, 30e3),
      # Throughout we assume that the minimum amount of super guarantee contributions are made. 
      super_income_in_excess_of_cap = pmax(0, 
                                           Rptbl_Empr_spr_cont_amt + Non_emp_spr_amt + super.guarantee.rate * Sw_amt - prev_cap),
      prv_concess_contr_amt = pmin(prev_cap, Rptbl_Empr_spr_cont_amt + Non_emp_spr_amt + super.guarantee.rate * Sw_amt),
      prv_tax_payable_on_concess_contr = 0.15 * prv_concess_contr_amt
      )
  
  if(div293){
    .tax201415 %<>%
      mutate(
        # https://www.ato.gov.au/uploadedFiles/Content/TPALS/downloads/Division-293-scenario-table-v2.pdf
        # Technically the surchargeable income includes amounts on which family trust distribution tax has been paid. This has
        # been omitted, meaning that some individuals' div 293 tax burden is underestimated.
        prv_tax_payable_on_concess_contr = 0.15 * prv_concess_contr_amt,
        prv_surchargeable_income293 = Taxable_Income + Net_fincl_invstmt_lss_amt - pmin(Net_rent_amt, 0) + Rep_frng_ben_amt,
        # We assume that low tax contributions cannot be negative.
        prv_low_tax_contributions293 = pmax(0, prv_concess_contr_amt - super_income_in_excess_of_cap),
        prv_div293_income = prv_surchargeable_income293 + prv_low_tax_contributions293,
        prv_div293_tax = ifelse(prv_div293_income > 300e3, 
                                0.15 * pmin(prv_low_tax_contributions293, 
                                            pmax(prv_div293_income - 300e3, 0)),
                                0)
      )
  } else {
    if(!missing(div293.threshold))
      warning("div293.threshold is provided, but div293 = FALSE")
    
    .tax201415 %<>%
      mutate(prv_tax_payable_on_concess_contr = 0.15 * prv_concess_contr_amt,
             new_tax_payable_on_concess_contr = 0.15 * new_concess_contr_amt,
             prv_div293_tax = 0,
             new_div293_tax = 0)
  }
  
  .tax201415 %<>%
    mutate(
      total_tax = new_tax - rebate,
      extra_tax_on_contr_if_no_concession = new_tax - .income_tax(Taxable_Income) -  prv_tax_payable_on_concess_contr - prv_div293_tax,
      extra_tax_liability = new_tax - previous_tax - rebate - prv_tax_payable_on_concess_contr - prv_div293_tax
    )
  
  difference <- 
    .tax201415 %$%
    sum(extra_tax_liability) * 50 * lf_inflator(to_date = "2015-06-30")
  .tax201415 <<- .tax201415
  return(difference)
}

.tax201415 %>% group_by(TXD = ntile(Taxable_Income, 10)) %>% summarise(mean.etl = mean(extra_tax_on_contr_if_no_concession)) %>% grplot(aes(x = factor(TXD), y = mean.etl)) + geom_bar(stat = "identity")
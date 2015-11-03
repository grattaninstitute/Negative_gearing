

henry_coster <- function(marginal_rate_subtractor = 0.20, super.guarantee.rate = 0.095){
  tax201415 %>%
    mutate(income_tax_payable = income_tax(Taxable_Income, fy.year = "2017-18"),
           current_tax_on_contributions = ifelse(Taxable_Income > 300e3, 0.30, 0.15) * (Rptbl_Empr_spr_cont_amt + Non_emp_spr_amt + pmin(50e3, super.guarantee.rate * Sw_amt)),
           marginal_rate = income_tax(Taxable_Income + 1, fy.year = "2017-18") - income_tax(Taxable_Income, fy.year = "2017-18")) %>%
    mutate(contributions_tax_henry = (marginal_rate - marginal_rate_subtractor) * (Rptbl_Empr_spr_cont_amt + Non_emp_spr_amt + pmin(50e3, super.guarantee.rate * Sw_amt))) %$%
    {(sum(contributions_tax_henry - current_tax_on_contributions) * 50 * lf_inflator(to_date = "2015-06-30"))} %>% 
      return
}
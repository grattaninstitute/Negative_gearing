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
         Taxable_Income = Taxable_Income * wage_inflator(from_fy = "2012-13", to_fy = "2013-14")^2) %>%
  select(Sw_amt, Rptbl_Empr_spr_cont_amt, Non_emp_spr_amt, Taxable_Income,
         age_range, Net_fincl_invstmt_lss_amt, Net_rent_amt, Rep_frng_ben_amt,
         Net_PT_PP_dsn, Net_PT_NPP_dsn)

# ====         
revenue_from_cap <- function(cap = 30000, cap2 = 35000, age_based_cap = FALSE, div293 = TRUE, div293.threshold = 300e3, super.guarantee.rate = 0.095){
  .tax201415 <- 
    tax201415 %>%
    # 50 year olds and over get a cap of 35e3
    mutate(prev_cap = ifelse(age_range <= 4, 35e3, 30e3)) %>%
    mutate(
      # Throughout we assume that the minimum amount of super guarantee contributions are made. 
      super_income_in_excess_of_cap = pmax(0, Rptbl_Empr_spr_cont_amt + Non_emp_spr_amt + super.guarantee.rate * Sw_amt - prev_cap),
      # the minimum of the cap applicable or the contributions actually made
      prv_concess_contr_amt = pmin(prev_cap, Rptbl_Empr_spr_cont_amt + Non_emp_spr_amt + super.guarantee.rate * Sw_amt),
      new_concess_contr_amt = pmin(ifelse(age_range <= 4, 
                                          ifelse(age_based_cap, cap2, cap), 
                                          cap), 
                                   #
                                   Rptbl_Empr_spr_cont_amt + Non_emp_spr_amt + super.guarantee.rate * Sw_amt),
      new_super_income_in_excess_of_cap = pmax(0, Rptbl_Empr_spr_cont_amt + Non_emp_spr_amt + super.guarantee.rate * Sw_amt - ifelse(age_range <= 4, ifelse(age_based_cap, cap2, cap), cap)),
      
      new_Taxable_Income = Taxable_Income - super_income_in_excess_of_cap + new_super_income_in_excess_of_cap,
      #
      revenue_due_to_Taxable_Income = income_tax(new_Taxable_Income, fy.year = "2015-16", include.temp.budget.repair.levy = FALSE) - income_tax(Taxable_Income, fy.year = "2015-16", include.temp.budget.repair.levy = FALSE)
      )
  
  if(div293){
      .tax201415 %<>%
      mutate(
        prv_tax_payable_on_concess_contr = 0.15 * prv_concess_contr_amt,
        new_tax_payable_on_concess_contr = 0.15 * new_concess_contr_amt,
        
        # https://www.ato.gov.au/uploadedFiles/Content/TPALS/downloads/Division-293-scenario-table-v2.pdf
        # Technically the surchargeable income includes amounts on which family trust distribution tax has been paid. This has
        # been omitted, meaning that some individuals' div 293 tax burden is underestimated.
        prv_surchargeable_income293 = Taxable_Income + Net_fincl_invstmt_lss_amt - pmin(Net_rent_amt, 0) + Rep_frng_ben_amt,
        # We assume that low tax contributions cannot be negative.
        prv_low_tax_contributions293 = pmax(0, prv_concess_contr_amt - super_income_in_excess_of_cap),
        prv_div293_income = prv_surchargeable_income293 + prv_low_tax_contributions293,
        prv_div293_tax = ifelse(prv_div293_income > 300e3, 
                                0.15 * pmin(prv_low_tax_contributions293, 
                                            pmax(prv_div293_income - 300e3, 0)),
                                0),
        
        new_surchargeable_income293 = new_Taxable_Income + Net_fincl_invstmt_lss_amt - pmin(Net_rent_amt, 0) + Rep_frng_ben_amt,
        new_low_tax_contributions293 = pmax(0, new_concess_contr_amt - new_super_income_in_excess_of_cap),
        new_div293_income = new_surchargeable_income293 + new_low_tax_contributions293,
        new_div293_tax = ifelse(new_div293_income > 300e3, 
                                0.15 * pmin(new_low_tax_contributions293, 
                                            pmax(new_div293_income - 300e3, 0)),
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
    
  .tax201415 <- 
    .tax201415 %>%
    mutate(
      diff = revenue_due_to_Taxable_Income + new_tax_payable_on_concess_contr - prv_tax_payable_on_concess_contr + new_div293_tax - prv_div293_tax,
      tax = income_tax(new_Taxable_Income, fy.year = "2015-16", include.temp.budget.repair.levy = FALSE) + new_tax_payable_on_concess_contr + new_div293_tax,
      
      extra_tax_on_contr_if_no_concession = 
        income_tax(new_Taxable_Income + new_concess_contr_amt,
                   fy.year = "2015-16", 
                   include.temp.budget.repair.levy = FALSE) - 
        income_tax(new_Taxable_Income, 
                   fy.year = "2015-16", 
                   include.temp.budget.repair.levy = FALSE) - 
        new_tax_payable_on_concess_contr - 
        new_div293_tax,
      
      tax_on_concession = new_tax_payable_on_concess_contr
    )
  # for debugging
  .tax201415 <<- .tax201415
  revenue <- sum(.tax201415$diff) * 50 * lf_inflator(to_date = "2015-06-30")
  return(revenue)
}

# ====
revenue_from_cap(age_based_cap = TRUE)/1e9
nocap.taxstats <- .tax201415
status_quo_tax_by_percentile <-
  .tax201415 %>%
  group_by(taxable_income_percentile = ntile(Taxable_Income, 100)) %>%
  summarise(mean_tax = mean(extra_tax_on_contr_if_no_concession))

rm(.tax201415)
gc(1,1)

revenue_from_cap(cap = 11000)/1e9
cap11k.taxstats <- .tax201415
cap11k_tax_by_percentile <- 
  .tax201415 %>%
  group_by(taxable_income_percentile = ntile(Taxable_Income, 100)) %>%
  summarise(mean_tax = mean(extra_tax_on_contr_if_no_concession))

bind_rows(status_quo_tax_by_percentile, cap11k_tax_by_percentile, .id = "id") %>% 
  grplot(aes(x = taxable_income_percentile, y = mean_tax, color = id)) + 
  geom_line(aes(group = id)) + theme(legend.position = c(0.2,0.7))

# ====
bind_rows(nocap.taxstats, cap11k.taxstats, .id = "hascap") %>%
  sample_frac(0.3) %>%
  mutate(hascap = ifelse(hascap == 2, "11,000 cap", "Status quo")) %>%
  group_by(
    Income_decile = ntile(Taxable_Income, 10)
  ) %>%
  mutate(
    decile_cuts = min(Taxable_Income)
  ) %>%
  grplot(aes(x = Taxable_Income, y = extra_tax_on_contr_if_no_concession, color = hascap)) + 
  geom_point(alpha = 0.3) + 
  geom_vline(aes(xintercept = decile_cuts), linetype = 2, color = theGrey) + 
  # stat_smooth(color = theGrey) + 
  facet_grid(hascap ~ .) +
  ggtitle("Contributions concessions of taxpayers") + 
  theme(strip.background = element_rect(fill = theGrey),
        strip.text = element_text(angle = 90, color = "white", face = "bold")) + 
  scale_y_continuous(label = grattan_dollar,
                     breaks = c(-3000, 0, 3000, 6000, 9000, 12000)) + 
  scale_x_continuous("Taxable income",
                     breaks = c(0, 20543, 37000,80000,180000,300000),
                     label = grattan_dollar,
                     limits = c(0,350e3))
# ====
dev.copy2pdf(file = paste0(Sys.Date(), "Benefit_due_to_contributions_tax_concession_reduced.pdf"), width = 27, height = 12)

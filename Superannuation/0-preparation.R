# setwd("..")

# Functions present in other scripts
require(grattan)
require(data.table)
require(dplyr) && require(magrittr)

tax201213 <- readr::read_csv("../IndividualSampleFile/2012-13/Sample_file_1213/2013 2% individuals sample file/2013_sample_file.csv")

tax201415 <- 
  tax201213 %>%
  mutate(Sw_amt = Sw_amt * wage_inflator(from_fy = "2012-13", to_fy = "2013-14")^3,
         Rptbl_Empr_spr_cont_amt = Rptbl_Empr_spr_cont_amt * wage_inflator(from_fy = "2012-13", to_fy = "2013-14")^3,
         Non_emp_spr_amt = Non_emp_spr_amt * wage_inflator(from_fy = "2012-13", to_fy = "2013-14")^3,
         #
         Taxable_Income = Taxable_Income * wage_inflator(from_fy = "2012-13", to_fy = "2013-14")^3) %>%
  select(Sw_amt, Rptbl_Empr_spr_cont_amt, Non_emp_spr_amt, Taxable_Income,
         age_range, Net_fincl_invstmt_lss_amt, Net_rent_amt, Rep_frng_ben_amt,
         Net_PT_PP_dsn, Net_PT_NPP_dsn)
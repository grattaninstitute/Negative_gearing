setwd("..")

# Functions present in other scripts

tax201213 <- get_sample_file()

tax201415 <- tax201213 %>%
  mutate(Sw_amt = wage_inflator(Sw_amt, from_fy = "2012-13", to_fy = "2013-14")^2,
         


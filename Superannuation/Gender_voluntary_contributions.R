setwd("..")

age_mutate_ <- function(.data){
  .data %>%
    mutate(Age = car::recode(age_range, "0 = '70 and over';
                             1 = '65 to 69';
                             2 = '60 to 64';
                             3 = '55 to 59';
                             4 = '50 to 54';
                             5 = '45 to 49';
                             6 = '40 to 44';
                             7 = '35 to 39';
                             8 = '30 to 34';
                             9 = '25 to 29';
                             10 = '20 to 24';
                             11 = 'under 20'")) %>%
    mutate(Age =  factor(Age,
                         levels = c("under 20",
                                    '20 to 24',
                                    '25 to 29',
                                    '30 to 34',
                                    '35 to 39',
                                    '40 to 44',
                                    '45 to 49',
                                    '50 to 54',
                                    '55 to 59',
                                    '60 to 64',
                                    '65 to 69',
                                    '70 and over'),
                         labels = gsub("\\s", "\n",
                                       c("under 20",
                                         '20 to 24',
                                         '25 to 29',
                                         '30 to 34',
                                         '35 to 39',
                                         '40 to 44',
                                         '45 to 49',
                                         '50 to 54',
                                         '55 to 59',
                                         '60 to 64',
                                         '65 to 69',
                                         '70 and over')),
                         ordered = T))
}

tax201213 <- get_sample_file() 

# ====
ten.thousand <- cpi_inflator(10e3, from_fy = "2014-15", to_fy = "2012-13")

tax201213 %>% 
  age_mutate_ %>%
  group_by(Gender = ifelse(as.logical(Gender), "Female", "Male"), 
           `Taxable income decile` = factor(ntile(Taxable_Income, 10))) %>%
  summarise(`Voluntary contributions` = sum(Non_emp_spr_amt + Rptbl_Empr_spr_cont_amt  >  ten.thousand),
            `Other contributions` = sum(0.095 * wage_inflator(Sw_amt, from_fy = "2012-13", to_fy = "2013-14"),  
                                          # + Non_emp_spr_amt + Rptbl_Empr_spr_cont_amt  
                                        >  ten.thousand)) %>% 
  gather(Contribution_type, number, `Voluntary contributions`, `Other contributions`) %>%
  mutate(`Contribution type` = factor(Contribution_type, levels = rev(levels(Contribution_type)))) %>%
  ungroup %>%
  # 2% sample file
  mutate(number = 50 * number) %>%
  grplot(aes(x = `Taxable income decile`, y = number, fill = `Contribution type`)) +
  geom_bar(stat = "identity") + 
  facet_grid(~Gender) + 
  scale_y_continuous(breaks = 2*(1:4) * 100e3, label=comma,expand = c(0,0)) + 
  guides(fill = guide_legend(override.aes = list(linetype = 0))) + 
  theme(legend.position = c(0.1,0.8))




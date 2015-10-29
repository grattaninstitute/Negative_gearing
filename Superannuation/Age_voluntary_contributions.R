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

tax201213 <- get_sample_file(year = 2013)
ten.thousand <- cpi_inflator(10e3, from_fy = "2012-13", to_fy = "2014-15")

tax201213 %>%
  age_mutate_() %>%
  mutate(Age_grouped = gsub("([0-9])([05])(.*)to(.*)([0-9])([49])", "\\10\\3to\\4\\59", Age)) %>%
  mutate(`Voluntary contributions` = cpi_inflator(Non_emp_spr_amt + Rptbl_Empr_spr_cont_amt, from_fy = "2012-13", to_fy = "2014-15")) %>%
  filter(`Voluntary contributions` > ten.thousand) %>%
  group_by(Age_grouped) %>%
  summarise(total_voluntary_contributions = sum(`Voluntary contributions`)) %>%
  arrange(Age_grouped) %$%
  pie(x = total_voluntary_contributions, labels = Age_grouped, clockwise = TRUE)
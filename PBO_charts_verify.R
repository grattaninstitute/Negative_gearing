# GST Rnw file should be loaded as initialization


# figure 3--1 ====
hes10hh %>% 
  group_by(disp_income_decile = .bincode(Current_weekly_HH_disposable_income, breaks = disp.inc.deciles, include.lowest = TRUE)) %>% 
  summarise(income = weighted.mean(Current_weekly_HH_disposable_income, Weight_HH_HES), 
            expend = weighted.mean(Total_goods_and_services_expenditure_HES_only, Weight_HH_HES)) %>% 
  
  gather(type, amount, income:expend) %>% mutate(amount = cpi_inflator(amount, from_fy = "2009-10", to_fy = "2014-15")) %>% 
  grplot(aes(x = factor(disp_income_decile), y = amount, fill = rev(type))) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_y_continuous(label=grattan_dollar, breaks = (0:8)*500)

# figure 3--2A ====
hes10hh %>%
  mutate(savings =  Current_weekly_HH_disposable_income - Total_goods_and_services_expenditure_HES_only) %>%
  group_by(disp_income_decile = .bincode(Current_weekly_HH_disposable_income, breaks = disp.inc.deciles, include.lowest = TRUE)) %>%
  summarise(avg_savings = weighted.mean(savings, Weight_HH_HES),
            avg_savings_rate = weighted.mean(savings / Current_weekly_HH_disposable_income, Weight_HH_HES)) %>%
  
  grplot(aes(x = factor(disp_income_decile),
             y = avg_savings)) + 
  geom_bar(stat = "identity", position = "dodge")

# ====
hes10hh %>%
  mutate(savings =  Current_weekly_HH_disposable_income - Total_goods_and_services_expenditure_HES_only) %>%
  group_by(disp_income_decile = .bincode(Current_weekly_HH_disposable_income, breaks = disp.inc.deciles, include.lowest = TRUE)) %>%
  summarise(avg_savings = weighted.mean(savings, Weight_HH_HES),
            avg_savings_rate = sum(savings * Weight_HH_HES) / sum(Current_weekly_HH_disposable_income * Weight_HH_HES)) %>%
  
  grplot(aes(x = factor(disp_income_decile),
             y = avg_savings_rate)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(limits=c(-1.5,1.5),
                     breaks = seq(-1.5,1.5,0.5),
                     label = percent)
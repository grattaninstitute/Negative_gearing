# Combining

source("0-preparation.R")
source("Reduce_concessions_cap_to_11k.R")
source("Henry_contribution_tax.R")
source("ALP_30pc_tax_over_115k.R")

revenue_from_cap(cap = 30e3, cap2 = 35e3, age_based_cap = TRUE) / 1e9
nocap.taxstats <- .tax201415

revenue_from_cap(cap = 11e3, age_based_cap = FALSE) / 1e9
cap11k.taxstats <- .tax201415

henry_coster(marginal_rate_subtractor = 0.15) / 1e9
henry15pc.taxstats <- .tax201415

henry_coster(marginal_rate_subtractor = 0.20) / 1e9
henry20pc.taxstats <- .tax201415

revenue_from_30pc_tax_over_115k()/1e9
ALP30pc.taxstats <- ALP_30pc_over_115k


extra_tax_by_certain_percentiles <- function(taxstats){
  taxstats %>% 
    group_by(taxable_income_percentile = ntile(Taxable_Income, 100)) %>%
    summarise(mean_concession = mean(extra_tax_on_contr_if_no_concession))
}

lapply(list(nocap.taxstats, cap11k.taxstats, henry15pc.taxstats, henry20pc.taxstats, ALP30pc.taxstats), 
       extra_tax_by_certain_percentiles) %>%
  bind_rows(.id = "id") %>%
  inner_join(., 
             data.table(id = as.character(1:5), 
                        policy = c("nocap", "cap11k", "henry15pc", "henry20pc", "ALP30pc"))) %>%
  mutate(policy = factor(policy, levels = c("nocap", "cap11k", "ALP30pc", "henry20pc", "henry15pc"))) %>%
  filter(taxable_income_percentile %in% (c(0:9)*10) | taxable_income_percentile == 95 | taxable_income_percentile == 99) %>%
  # filter(taxable_income_millenial %in% (c(0:9)*100) | taxable_income_millenial == 950 | taxable_income_millenial == 990 | taxable_income_millenial == 995) %>%
  #
  grplot(aes(x = factor(taxable_income_percentile),
             y = mean_concession, 
             fill = policy)) + 
  geom_bar(stat = "identity",
           position = "dodge") + 
  theme(legend.position = c(0.2, 0.7)) + 
  scale_fill_manual(values = c("black", gpal(4))) +
  scale_y_continuous(breaks = (0:6)*1000)


# =====
.predata <- 
  list(nocap.taxstats, cap11k.taxstats, henry15pc.taxstats, henry20pc.taxstats, ALP30pc.taxstats) %>%
  bind_rows(.id = "id") %>%
  inner_join(., 
             data.table(id = as.character(1:5), 
                        policy = c("nocap", "cap11k", "henry15pc", "henry20pc", "ALP30pc"))) %>%
  sample_frac(1)

.predata %>%
  sample_frac(0.5) %>%
  group_by(
    Income_decile = ntile(Taxable_Income, 10)
  ) %>%
  mutate(
    decile_cuts = min(Taxable_Income)
  ) %>%
  grplot(aes(x = Taxable_Income, y = extra_tax_on_contr_if_no_concession, color = policy)) + 
  geom_point(alpha = 0.1) + 
  geom_vline(aes(xintercept = decile_cuts), linetype = 2, color = theGrey) + 
  # stat_smooth(color = theGrey) + 
  facet_grid(~policy) +
  ggtitle("Contributions concessions of taxpayers") + 
  theme(strip.background = element_rect(fill = theGrey),
        strip.text = element_text(angle = 0, color = "white", face = "bold")) + 
  scale_y_continuous(label = grattan_dollar,
                     limits = c(-3000, 12000),
                     breaks = c(-3000, 0, 3000, 6000, 9000, 12000, 15000)) + 
  scale_x_continuous("Taxable income",
                     breaks = c(0, 20543, 37000,80000,180000,300000),
                     label = grattan_dollar,
                     limits = c(0,350e3)) + 
  scale_color_manual(values = c(gpal(5)[1:4], "black"))
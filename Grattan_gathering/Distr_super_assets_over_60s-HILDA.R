library(survey)
library(foreign)
library(data.table)
library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(scales)
library(grattan)

hilda_master <- 
  read.dta("../HILDA/Master_m130c.dta") %>%
  data.table
  
hilda <- read.dta("../HILDA/Household_j130c.dta")
hilda <- data.table(hilda)

hilda %>%
  select(
    jhhrhid,
    jhhwth,
    jhwsupei, # Superannuation 
    jhwfini, # financial assets
    jhwassei, # other assets
    age = jhgage1
  ) %>% 
  mutate(
    prop_super = jhwfini / jhwassei
  ) %>%
#   group_by(age) %>%
#   summarise(prop_super = weighted.mean(prop_super, jhhwth)) %>%
  #
  grplot(aes(x = age, y = prop_super, group = 1, weight = jhhwth)) + 
  stat_smooth(color = Orange, size = 1.5) +
  scale_y_continuous(label=percent)

hilda %>%
  grplot(aes(x = jhgage1, y = jhwsupei)) + 
  geom_point(aes(alpha = jhhwth/max(jhhwth)))

# ====
hilda %>%
  svydesign(data = ., weights = ~jhhwth, ids = ~jhhrhid) %>%
  svyquantile(~jhwassei, design = ., quantiles = (0:100)/100) ->
  asset_percentiles

hilda %>%
  svydesign(data = ., weights = ~jhhwth, ids = ~jhhrhid) %>%
  svyquantile(~jhwassei, design = ., quantiles = (0:10)/10) ->
  asset_deciles

hilda %>%
  svydesign(data = ., weights = ~jhhwth, ids = ~jhhrhid) %>%
  svyquantile(~jhwsupei, design = ., quantiles = (0:100)/100) ->
  super_percentiles

# ====
hilda %>%
  select(
    jhhrhid,
    jhhwth,
    jhwsupei, # Superannuation 
    jhwfini, # financial assets
    jhwassei, # other assets
    age = jhgage1
  ) %>% 
  mutate(
    prop_super = jhwsupei / jhwassei,
    age_group = factor(cut(age, breaks = c(-Inf, 17.5, 64, Inf)), 
                       labels = c("underage", "Working", "Retired")) 
  ) %>%
  #   group_by(age) %>%
  #   summarise(prop_super = weighted.mean(prop_super, jhhwth)) %>%
  #
  filter(age > 18) %>%
  grplot(aes(x = jhwassei, y = prop_super, weight = jhhwth)) + 
  # stat_smooth(color = Orange, size = 1.5) +
  stat_smooth(aes(color = age_group, group = age_group),
              size = 1.5, fill = NA) +
  scale_y_continuous(label=percent) + 
  scale_color_manual(values = gpal(2, TRUE)) + 
  scale_x_continuous(label=dollar) + 
  theme(legend.position = c(0.7, 0.8)) 

# ====
hilda %>%
  mutate(
    prop_super = jhwsupei / jhwassei,
    age_group = factor(cut(jhgage1, breaks = c(-Inf, 17.5, 64, Inf))) 
  ) %>%
  filter(jhgage1 >= 18) %>%
  group_by(age_group,
           super_percentile = .bincode(jhwsupei, breaks = super_percentiles, include.lowest = TRUE)) %>%
  summarise(prop_super = weighted.mean(prop_super, jhhwth)) %>%
  #
  grplot(aes(x = super_percentile, y = prop_super, color = age_group, group = age_group)) + 
  stat_smooth(size = 1.5) + 
  scale_y_continuous(label=percent) + 
  scale_color_manual(values = gpal(2, TRUE)) + 
  theme(legend.position = c(0.7, 0.8))


# ====
hilda %>%
  mutate(
    prop_super = jhwsupei / jhwassei,
    age_group = factor(cut(jhgage1, breaks = c(-Inf, 17.5, 64, Inf))) 
  ) %>%
  filter(jhgage1 >= 18) %>%
  group_by(age_group,
           asset_percentile = .bincode(jhwassei, breaks = super_percentiles, include.lowest = TRUE)) %>%
  summarise(prop_super = weighted.mean(prop_super, jhhwth)) %>%
  #
  grplot(aes(x = super_percentile, y = prop_super, color = age_group, group = age_group)) + 
  stat_smooth(size = 1.5) + 
  scale_y_continuous(label=percent) + 
  scale_color_manual(values = gpal(2, TRUE)) + 
  theme(legend.position = c(0.7, 0.8))

# ====
hilda %>%
  filter(jhgage1 >= 18) %>%
  filter(jhwassei > 0) %>%
  grplot(aes(x = jhwsupei / jhwassei, y = ..count.., weight = jhhwth)) + 
  geom_histogram() + 
  scale_x_continuous(label=percent) 

## Why so many people with zero super?
# ====
hilda %>%
  filter(!is.na(jhwassei)) %>%
  mutate(asset_percentile = .bincode(jhwassei, breaks = super_percentiles, include.lowest = TRUE),
         low_assets = jhwassei  < 50e3) %>% 
  filter(jhgage1 >= 18) %>%
  filter(jhwassei > 0) %>%
  grplot(aes(x = jhwsupei / jhwassei, y = ..count.., weight = jhhwth, fill = jhgage1 > 64)) +
  facet_grid(low_assets ~ .) +
  geom_histogram() + 
  theme(legend.position = c(0.8, 0.8)) + 
  scale_x_continuous(label=percent) 
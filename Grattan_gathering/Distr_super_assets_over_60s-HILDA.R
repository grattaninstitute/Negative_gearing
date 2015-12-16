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
  select(
    jhhrhid,
    jhhwth,
    jhwsupei, # Superannuation 
    jhwfini, # financial assets
    jhwassei, # other assets
    age = jhgage1
  ) %>% 
  mutate(
    prop_super = jhwfini / jhwassei,
    age_group = factor(cut(age, breaks = c(-Inf, 18, 64, Inf))) 
  ) %>%
  #   group_by(age) %>%
  #   summarise(prop_super = weighted.mean(prop_super, jhhwth)) %>%
  #
  grplot(aes(x = jhwassei, y = prop_super, weight = jhhwth)) + 
  # stat_smooth(color = Orange, size = 1.5) +
  stat_smooth(aes(color = age_group, group = age_group),
              size = 1.5, fill = NA) + 
  scale_y_continuous(label=percent) + 
  scale_color_manual(values = c(theGrey, gpal(4))) + 
  theme(legend.position = c(0.7, 0.8))


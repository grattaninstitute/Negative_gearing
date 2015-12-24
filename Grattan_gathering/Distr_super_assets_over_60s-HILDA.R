library(survey)
library(foreign)
library(data.table)
library(dplyr)
library(magrittr)
library(tidyr)
library(readr)
library(readxl)
library(ggplot2)
library(scales)
library(grattan)

hilda_master <- 
  read.dta("../HILDA/Master_m130c.dta") %>%
  data.table
  
hilda <- read.dta("../HILDA/Household_j130c.dta")
hilda <- data.table(hilda)

hildaEp <- read.dta("../HILDA/Eperson_j130c.dta")
hildaEp <- data.table(hildaEp)

hilda.HH.EP.merge <- 
  data.table:::merge.data.table(hilda, hildaEp, by = "jhhrhid", suffixes = c("_hh", "_ep"))

data_dictionary <-
    read_excel("../HILDA/data_dictionary.xlsx", sheet = "For_input")

data_dictionary.wealth <- 
    read_excel("../HILDA/data_dictionary.xlsx", sheet = "Wealth_data_dictionary", col_types = rep("text", 17))

hilda %>%
  select(
    jhhrhid,
    jhhwth,
    jhwsupei, # Superannuation 
    jhwfini, # financial assets
    jhwassei, # other assets
    age = hgage,
    age1 = jhgage1
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

# Enumerated person ====
hildaEp %>%
  grplot(aes(x = (jpwsupri + jpwsupwi)/(jhwnwip - jhwnwin))) + 
  geom_histogram()


# Both ====
hilda.HH.EP.merge %>%
  filter(jhgage >= 18) %>%
  mutate(asset_percentile  = .bincode(jhwassei, breaks = asset_percentiles, include.lowest = TRUE),
         super_percentile  = .bincode(jhwsupei, breaks = super_percentiles, include.lowest = TRUE)
         # ,income_percentile = .bincode()
         ,super_prop_of_asset = jhwsupei / jhwassei
         ) %>%
  grplot(aes(x = super_prop_of_asset)) + 
  geom_histogram(aes(fill = jhgage >= 60 & jhgage <= 70), binwidth = 0.10) + 
  scale_x_continuous(label = percent, breaks = (0:5)/5,  limits = c(0,1)) + 
  scale_y_continuous(label = grattan_dollar)

# =====
# SIH 2013-14
sih201314 <- 
  read.dta("../SIH/Stata files/sih13bh.dta") %>%
  data.table


sih201314.var.listing <-  
  read_excel("../SIH/SIH_201314_variable_listing.xlsx", sheet = "Household") %>%
  filter(!is.na(Identifier)) %>%
  data.table %>%
  setnames(old = c("Label and categories", "Identifier"), c("Label", "Identifier")) %>% 
  mutate(new_label = gsub("[^a-zA-Z0-9]+", "_",
                          gsub("(\\s+$)|(^\\s+)", "", Label)),
         # Cleanse of random vars
         Identifier = gsub("(^\\s+)|(\\s+$)", 
                           "",
                           Identifier)) %>%
  setkey(Identifier)

# Some variables haven't been coded in a normal form, 
# i.e. they are ranges of variables or describe the variable
# rather than being the explicit variable.  Accordingly, 
# we exclude them from our table.

sih201314.var.listing.subset <-
  sih201314.var.listing %>%
  filter(!grepl("\\s", Identifier), 
         (Identifier %in% names(sih201314)))

sih201314 %>% 
  setnames(old = sih201314.var.listing.subset$Identifier, 
           new = sih201314.var.listing.subset$new_label)


# ====
# Person file preparation
sih201314.person <-
  read.dta("../SIH/Stata files/sih13bp.dta") %>%
  data.table

sih201314.person.var.names <- 
  read_excel("../SIH/SIH_201314_variable_listing.xlsx", sheet = "Person") %>%
  filter(!is.na(Identifier)) %>%
  data.table %>%
  setnames(old = c("Label and categories", "Identifier"), c("Label", "Identifier")) %>% 
  mutate(new_label = gsub("[^a-zA-Z0-9]+", "_",
                          gsub("(\\s+$)|(^\\s+)", "", Label)),
         # Cleanse of random vars
         Identifier = gsub("(^\\s+)|(\\s+$)", 
                           "",
                           Identifier)) %>%
  setkey(Identifier)


sih201314.var.listing.subset.person <-
  sih201314.person.var.names %>%
  filter(!grepl("\\s", Identifier), 
         (Identifier %in% names(sih201314.person)))

sih201314.person %>% 
  setnames(old = c(sih201314.var.listing.subset.person$Identifier
                   , "AGEBC"), # age mislabled in original
           new = c(sih201314.var.listing.subset.person$new_label
                   , "Age_of_person"))

ages.by.hh <- 
  sih201314.person %>%
  select(Unique_household_number,
         Person_number_within_each_income_unit,
         Age_of_person)

max.age.by.hh <- 
  ages.by.hh %>%
  group_by(Unique_household_number) %>%
  summarise(max_age = max(Age_of_person))
  

# ====
sih201314 %>%
  #data.table:::merge.data.table(max.age.by.hh, by = "Unique_household_number") %>%
  mutate(total_super_balance = Balance_of_accounts_with_government_superannuation_funds_household_level + Balance_of_accounts_with_non_government_superannuation_funds_household_level,
         prop_assets_in_super = total_super_balance / Net_wealth_of_household) %>%
  grplot(aes(x = prop_assets_in_super, y = ..count.., weights = Weight_HH_SIH_)) + 
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(label=comma)

# ====
# Relationship betwen prop assets in super and net wealth
sih201314 %>%
  #data.table:::merge.data.table(ages.by.hh, by = "Unique_household_number") %>%
  mutate(total_super_balance = Balance_of_accounts_with_government_superannuation_funds_household_level + Balance_of_accounts_with_non_government_superannuation_funds_household_level,
         prop_assets_in_super = total_super_balance / Net_wealth_of_household) %>%
  #
  ggplot(aes(x = prop_assets_in_super, y = Net_wealth_of_household, weight = Weight_HH_SIH_)) + 
  geom_bin2d(binwidth = c(0.01, 1000)) + 
  scale_x_continuous(limits = c(0,1)) + 
  scale_y_continuous(limits = c(0,2e5))

# Jack all.

# ==== 
# What about income?
sih201314 %>%
  #data.table:::merge.data.table(ages.by.hh, by = "Unique_household_number") %>%
  mutate(total_super_balance = Balance_of_accounts_with_government_superannuation_funds_household_level + Balance_of_accounts_with_non_government_superannuation_funds_household_level,
         prop_assets_in_super = total_super_balance / Net_wealth_of_household) %>%
  #
  ggplot(aes(x = prop_assets_in_super, y = Total_current_weekly_HH_income_from_all_sources, weight = Weight_HH_SIH_)) +
  stat_bin2d(binwidth = c(0.01, 1000)) + 
  scale_x_continuous(limits = c(0,1))

# ====
sih201314 %>%
  data.table:::merge.data.table(ages.by.hh, by = "Unique_household_number") %>%
  mutate(total_super_balance = Balance_of_accounts_with_government_superannuation_funds_household_level + Balance_of_accounts_with_non_government_superannuation_funds_household_level,
         prop_assets_in_super = total_super_balance / Net_wealth_of_household) %>%
  filter(prop_assets_in_super < 0.01)

## Tree diagram
# ====
rpart.zero.super <- 
  sih201314 %>%
  mutate(total_super_balance = Balance_of_accounts_with_government_superannuation_funds_household_level + Balance_of_accounts_with_non_government_superannuation_funds_household_level,
         prop_assets_in_super = total_super_balance / Net_wealth_of_household) %>%
  select(-Unique_household_number, -contains("supera"), -prop_assets_in_super) %>%
  rpart(total_super_balance == 0 ~ ., data = ., weights = Weight_HH_SIH_) 

rpart.zero.super %$%
  as.list(variable.importance) %>%
  data.frame() %>%
  mutate(id = 1:n()) %>%
  gather(variable, importance, -id) %>%
  grplot(aes(x = variable, y = importance)) + 
  geom_bar(stat = "identity") +
  coord_flip()

# collinearity much?
sih201314 %>% count(NOEMPHBC, NOEARNBC) %>% grplot(aes(x = NOEMPHBC, y = NOEARNBC, alpha = n/max(n))) + geom_tile()

rpart.zero.super <- 
  sih201314 %>%
  mutate(total_super_balance = Balance_of_accounts_with_government_superannuation_funds_household_level + Balance_of_accounts_with_non_government_superannuation_funds_household_level,
         prop_assets_in_super = total_super_balance / Net_wealth_of_household) %>%
  select(-Unique_household_number, -contains("supera"), -prop_assets_in_super,
         -NOEARNBC) %>%
  rpart(total_super_balance == 0 ~ ., data = ., weights = Weight_HH_SIH_) 

rpart.zero.super %$%
  as.list(variable.importance) %>%
  data.frame() %>%
  mutate(id = 1:n()) %>%
  gather(variable, importance, -id) %>%
  grplot(aes(x = variable, y = importance)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = c(0,0)) + 
  coord_flip()


  
  
library(data.table)
library(dplyr)
library(survey)

# hilda.w14.hh <- foreign::read.dta("../HILDA/Wave14/Household_n140c.dta")
hilda.w14.hh.raw <- as.data.table(readRDS("../HILDA/Wave14/csv/Household_n140c.rds"))

metadata <- fread("../HILDA/csv/data_dictionary.csv")

# Some of the descriptions have caveats or 
bracketed.items <- 
  metadata %>% 
  filter(grepl("\\[", Description)) %$% 
  stringr::str_extract_all(string = Description, pattern = "\\[[a-zA-Z ]+\\]") %>%
  unlist %>%
  unique

name_decoder <-
  metadata %>%
  mutate(in_dollars = grepl("$", Description, fixed = TRUE),
         Description = gsub("($)", "", Description, fixed = TRUE),
         weighted.topcode = grepl("[weighted topcode]", Description, fixed = TRUE),
         Description = gsub("[weighted topcode]", "", Description, fixed = TRUE),
         derived.var = grepl("^DV[:] ", Description),
         Description = gsub("^DV[:] ", "", Description),
         imputed.var = grepl("[imputed]", Description, fixed = TRUE),
         Description = gsub("[imputed]", "", Description, fixed = TRUE),
         signed.var = grepl("positive values", Description, ignore.case = TRUE) | grepl("negative values", Description, ignore.case = TRUE),
         sign.of.var = ifelse(signed.var, 
                              ifelse(grepl("positive values", Description, ignore.case = TRUE), 
                                     1L, 
                                     -1L), 
                              NA_integer_),
         # positive values are inconsistently defined in the description
         # e.g [positive values*] Positive values
         Description = gsub("(positive values*)|(negative values*)", "", Description,
                            ignore.case = TRUE),
         SCQ.var = grepl("[SCQ]", Description, fixed = TRUE),
         Description = gsub("[SCQ]", "", Description, fixed = TRUE),
         estimated.var = grepl("[estimated]", Description, fixed = TRUE),
         Long_name = sub("_+$", "", 
                         gsub("[^0-9A-Za-z]+", 
                              "_", 
                              sub("\\s+$", "", Description))),
         Long_name = ifelse(imputed.var, paste0(Long_name, ".i"), Long_name),
         # we should still take note of positive/negative value to avoid two
         # vars having the same name
         Long_name = ifelse(is.na(sign.of.var),
                            Long_name,
                            ifelse(sign.of.var == 1, 
                            paste0(Long_name, ".p"),
                            paste0(Long_name, ".n")))
         ) %>%
  mutate(nVariable = paste0("n", Variable)) %>%
  filter(nVariable %in% names(hilda.w14.hh.raw))

hilda.w14.hh <- hilda.w14.hh.raw
setnames(hilda.w14.hh,
         old = name_decoder$nVariable, 
         new = name_decoder$Long_name)

# Remove missing values:
# Collapse them because I don't care why
numeric.cols <- sapply(hilda.w14.hh, is.numeric)
# If there are any negative numbers combined with missing values I swear
# to Hadley this script is over.
stopifnot(all(hilda.w14.hh[,numeric.cols, with = FALSE] >= -10))

for (k in which(numeric.cols)){
  set(hilda.w14.hh, j = k, value = ifelse(hilda.w14.hh[[k]] < 0, NA, hilda.w14.hh[[k]]))
}




hilda.w14.hh %>% 
  select(Household_wealth_Total_superannuation,
         Household_wealth_Total_superannuation.i,
         Household_Net_Worth.p,
         Household_Net_Worth.n,
         Household_population_weight) %>%
  mutate(prop_superannuation = Household_wealth_Total_superannuation / (Household_Net_Worth.p - Household_Net_Worth.n)) %>%
  grplot(aes(x = prop_superannuation, weight = Household_population_weight)) + 
  geom_histogram() +
  xlim(-1,1)


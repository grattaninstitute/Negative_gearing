library(data.table)
library(dplyr)
library(survey)

# hilda.w14.hh <- foreign::read.dta("../HILDA/Wave14/Household_n140c.dta")
hilda.w14.hh <- as.data.table(readRDS("../HILDA/Wave14/csv/Household_n140c.rds"))

metadata <- fread("../HILDA/csv/data_dictionary.csv")

# Some of the descriptions have caveats or 
bracketed.items <- 
  metadata %>% 
  filter(grepl("\\[", Description)) %$% 
  stringr::str_extract_all(string = Description, pattern = "\\[[a-zA-Z ]+\\]") %>%
  unlist %>%
  unique

desc.to.name <- 
  function(v){
    gsub(" - ", " ", v, fixed = TRUE)
  }



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
         sign.of.var = signed.var * ifelse(grepl("positive values", Description, ignore.case = TRUE), 1L, -1L),
         Description = gsub("(positive values*)|(negative values*)", "", Description,
                            ignore.case = TRUE),
         SCQ.var = grepl("[SCQ]", Description, fixed = TRUE),
         Description = gsub("[SCQ]", "", Description, fixed = TRUE),
         estimated.var = grepl("[estimated]", Description, fixed = TRUE),
         Long_name = gsub("[^0-9A-Za-z]", "_", Description)
         ) %>%
  select(-contains("Wave")) %>%
  filter(signed.var) %>%
  print

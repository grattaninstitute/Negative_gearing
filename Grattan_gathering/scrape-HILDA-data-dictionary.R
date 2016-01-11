library(httr)
library(rvest)
library(magrittr)
library(data.table)
library(readr)
library(foreign)

filename <- "../HILDA/Wave14/Eperson_n140c.dta"
hilda.w14.names <- 
  sub("^n", "", names(read.dta(filename)))
hilda.file <- stringr::str_extract(pattern = "(Household)|(Eperson)|(Rperson)|(Combined)",
                                   string = filename)

url <- 
  "https://www.melbourneinstitute.com/hildaddictionary/onlinedd/srchVarnameUsingCategoriesCrossWave.aspx"
hilda_session <- html_session(url)

get_metadata1 <- function(var){
  filled_form <- 
    hilda_session %>%
    html_form() %>%
    extract2(1) %>%
    set_values(DerivedVariableTextBox = var)
  
  hilda_session %>%
    submit_form(filled_form, submit = 'ImageButton1') %>%
    html_nodes(xpath = '//*[@id="gridviewQueryResults"]') %>%
    html_table(fill = TRUE) 
}

metadata.list <- lapply(as.list(hilda.w14.names), get_metadata1)
metadata.list.list <- lapply(metadata.list, function(lst) magrittr::extract2(lst, 1))
metadata.tbl <- rbindlist(metadata.list.list, use.names = TRUE, fill = TRUE)

# Inconsistent treatment of NAs: sometimes ""
for (j in 1:ncol(metadata.tbl)){
  set(metadata.tbl, 
      i = which(metadata.tbl[[j]] == ""), 
      j = j, 
      value = NA_character_)
}
metadata.tbl <- unique(metadata.tbl)

# Make our vars nice!
num.cols <- grep("^[0-9]+$", names(metadata.tbl), value = TRUE)
setnames(metadata.tbl,
         old = num.cols,
         new = paste0("Wave_", num.cols))
# Still some variables left. That's ok: we got what we needed.
# However, some variables were not returned:
metadata.tbl %>%
  select(Variable, Description, starts_with("Wave")) %>%
  filter(!grepl("^No results were found for search string", Variable)) %>%
  write_csv(path = paste0("../HILDA/csv/data_dictionary_", hilda.file, ".csv"))

metadata.tbl %>%
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
  ) 




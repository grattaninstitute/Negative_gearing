library(httr)
library(rvest)

hilda.w14.hh.names <- 
  sub("^n", "", names(fread("../HILDA/Wave14/csv/Household_n140c.csv", nrows = 5)))

url <- "https://www.melbourneinstitute.com/hildaddictionary/onlinedd/srchVarnameUsingCategoriesCrossWave.aspx"
hilda_session <- html_session(url)

get_metadata1 <- function(var){
  filled_form <- 
    hilda_session %>%
    html_form() %>%
    extract2(1) %>%
    set_values(DerivedVariableTextBox = var)
  
    hilda_session %>%
    submit_form(filled_form) %>%
    html_nodes(xpath = '//*[@id="gridviewQueryResults"]') %>%
    html_table(fill = TRUE)
}

metadata.list <- lapply(as.list(hilda.w14.hh.names), get_metadata1)
metadata.list.list <- lapply(metadata.list, function(lst) magrittr::extract2(lst, 1))
metadata.tbl <- rbindlist(metadata.list.list)

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
readr::write_csv(metadata.tbl[!grepl("^No results were found for search string", Variable)],
                 path = "../HILDA/csv/data_dictionary.csv")






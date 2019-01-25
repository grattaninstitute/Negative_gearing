

# First remove all but the CGT_
vapply(dir(path = ".", pattern = "\\.tex$"), function(x) {
  grepl("pattern", x) && file.remove(x)
}, FALSE)

install.packages("hutils", repos = "https://cran.rstudio.com", dependencies = TRUE, quiet = TRUE)
install.packages("devtools", repos = "https://cran.rstudio.com", dependencies = TRUE, quiet = TRUE)
install.packages("grattan", repos = "https://cran.rstudio.com", dependencies = TRUE, quiet = TRUE)
install.packages("TeXCheckR", repos = "https://cran.rstudio.com", dependencies = TRUE)

devtools::install_github(c("hutilscpp", "grattanCharts"), quiet = TRUE)

install.packages(c("taxstats", "taxstats1516"),
                 repos = "https://hughparsonage.github.io/drat",
                 type = "source")

packages_ <-
  c("bindrcpp", "hildaData", "hutils", "ggrepel", "testthat", 
    "magrittr", "tidyr", "usethis", "devtools", "expm", "Hmisc", 
    "Formula", "lattice", "foreign", "survey", "survival", "Matrix", 
    "grid", "zoo", "httr", "rsdmx", "readr", "openxlsx", "readxl", 
    "xtable", "grattan", "directlabels", "scales", "ggplot2", "gridExtra", 
    "dplyr", "haven", "devEMF", "knitr", 
    "data.table", "sessioninfo")
install.packages(packages_, repos = "https://cran.rstudio.com", dependencies = TRUE)

knitr::knit("CGT_and_neg_gearing_parent.Rnw")







# Parameter descriptions ----
filepath <- paste0(here::here(), "/inst/extdata/", "parameter_descriptions.csv")
parameter_descriptions <- read.table(filepath, sep = ";", header = TRUE, encoding = "UTF-8")

usethis::use_data(parameter_descriptions, overwrite = TRUE)

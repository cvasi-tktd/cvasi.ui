# Parameter descriptions ----
filepath <- file.path("inst/extdata", "parameter_descriptions.xlsx")
parameter_descriptions <- readxl::read_xlsx(filepath)

usethis::use_data(parameter_descriptions, overwrite = TRUE)

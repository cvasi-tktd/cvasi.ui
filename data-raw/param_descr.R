# Parameter descriptions ----
filepath <- file.path("data-raw", "parameter_descriptions.xlsx")
parameter_descriptions <- readxl::read_xlsx(filepath) %>%
  dplyr::mutate(lower.boundary=as.numeric(lower.boundary),
                upper.boundary=as.numeric(upper.boundary)) %>%
  as.data.frame()

usethis::use_data(parameter_descriptions, overwrite = TRUE)
rm(filepath, parameter_descriptions)

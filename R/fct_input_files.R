#' Import forcings from file
#' 
#' Import a text file with three c
#'
#' @param filepath path to file with forcings data
#' @param sep value seperator
#' @param forcing_col columnname that contains the 
#'
#' @return a list of forcings with a data.frame for each forcing
import_forcings <- function(filepath, 
                            sep = ";", 
                            time_col = "time", 
                            forcing_col = "forcing", 
                            value_col = "value"){
  if(!file.exists(filepath)) stop("File does not exist!")
  f_dat <- read.table(file = filepath, sep = sep, header = TRUE)
  
  if(!(time_col %in% colnames(f_dat))) stop(paste0("Column ",time_col, " does not exist."))
  if(!(forcing_col %in% colnames(f_dat))) stop(paste0("Column ",forcing_col, " does not exist."))
  if(!(value_col %in% colnames(f_dat))) stop(paste0("Column ",value_col, " does not exist."))
  
  f_dat_list <- split(f_dat, f_dat[,forcing_col])
  f_dat_list <- lapply(f_dat_list, function(x) x %>% dplyr::select(!all_of(forcing_col)) )
  f_dat_list
}


#' check if the imported forcings meet the requirements
#'
#' @param forcings the imported forcings data in long format
#' @param expected_forcings the forcings required as a character vector
#' @param forcing_col the column of the forcings data in which the forcing names are listed
#'
#' @return NULL if requirements are met; error if not
#'
#' @examples
#' \dontrun{
#' f_dat <- list(
#'                rad = data.frame(time = 1:10, value = rnorm(10)),
#'                temp = data.frame(time = 1:10, value = rnorm(10))
#'                )
#' exp_f <- c("rad", "temp")
#' check_forcings_ts(f_dat, exp_f)
#' 
#' exp_f <- c("irr", "tmp")
#' check_forcings_ts(f_dat, exp_f)
#' }
check_forcings_ts <- function(forcings, expected_forcings){

  available_forcings <- names(forcings)
  if(!all( available_forcings %in% expected_forcings)){
    stop(paste0("Not all required forcings (",
                paste(expected_forcings, collapse = ", "),
                ") are available. The data contains forcings on ",
                paste(available_forcings, collapse = ", ")
                ))
  }
  return(NULL)
}

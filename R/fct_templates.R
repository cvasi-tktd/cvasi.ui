#' create exposure file template
#'
#' @param filepath file destination; use txt file ending.
#' @param trials should a 'trial' column be created?
#' @param example_data should the data contain example values? 
#' If FALSE only NA will be included
#'
#' @return the path to the example file if a filepath was given. 
#' If filepath is missing the data is returned as data.frame
#' 
#' @importFrom utils write.table
#' @export
template_exposure <- function(filepath, trials = FALSE, example_data = FALSE){
  
  if (example_data){
    # example data ----
    times <- 0:21
    c0 <- 1
    r <- 0.05
    conc <- round(c0*(1-r)^times,4) # exponential decay as example data.
    trial <- "T1"
  } else {
    times <- NA
    conc <- NA
    trial <- NA
  }
  
  # data in data.frame ----
  if (trials){
    o <- data.frame(time = times, conc = conc, trial = trial)
  } else {
    o <- data.frame(time = times, conc = conc)
  }
  
  # export example data ----
  if (rlang::is_missing(filepath)){
    return(o)
  } else {
    f_ext <- tools::file_ext(filepath)
    if (f_ext == ""){
      filepath <- paste0(filepath,".txt")
    } else if (f_ext != "txt"){
      stop("Use 'txt' as file extension.")
    }
    write.table(o, file = filepath, 
                sep = "\t", row.names = FALSE, quote = FALSE)
    return(filepath)
  }
  
}

#' create exposure file template
#'
#' @param filepath file destination; use txt file ending.
#' @param forcings a vector of strings indicating the 
#'
#' @return the path to the example file if a filepath was given. 
#' If filepath is missing the data is returned as data.frame
#' 
#' @importFrom utils write.table
#' @export
template_forcings <- function(filepath, forcings){
  
  # Create data.frame with only NA
  o <- expand.grid(time = 0:21, forcing = forcings, value = NA,
                   KEEP.OUT.ATTRS = FALSE, 
                   stringsAsFactors = FALSE)
  
  # export example data ----
  if (rlang::is_missing(filepath)){
    return(o)
  } else {
    f_ext <- tools::file_ext(filepath)
    if (f_ext == ""){
      filepath <- paste0(filepath,".csv")
    } else if (f_ext != "csv"){
      stop("Use 'csv' as file extension.")
    }
    write.table(o, file = filepath, 
                sep = ";", row.names = FALSE, quote = FALSE)
    return(filepath)
  }
}

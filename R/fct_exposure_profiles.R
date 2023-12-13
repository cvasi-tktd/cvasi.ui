#' Read an exposure profile 
#'
#' Read a single exposure profile at 'pathtofile'
#'
#' @param pathtofile the path where the exposure profile is found
#'
#' @importFrom attempt attempt
#'
#' @return the exposure profile
#' @export
read_single_exposure <- function(pathtofile, sep = "\t"){
  profile <- read.table(pathtofile, header = TRUE, sep = sep) %>% attempt::attempt(msg=paste0("Could not read file ",pathtofile))

  if (class(profile)=="try-error")
    profile <- NULL
  
  checkedProfile <- check_profile(profile)
  
  if(length(checkedProfile) & !("trial" %in% colnames(checkedProfile))){
    checkedProfile[,"trial"] <- "1"
  } else { 
    checkedProfile[,"trial"] <- as.character(checkedProfile[,"trial"])
  }
  
  return(checkedProfile)
}


#' Read exposure profiles
#' 
#' Read several exposure profiles at their corresponding pathes at 'pathtofiles'
#'
#' @param pathtofiles 
#' @param profileNames 
#'
#' @return list of exposure profiles
#' @export
read_exposure_profiles <- function(pathtofiles, profileNames=NA){
  if ( any(sapply(profileNames, is.na)) ) {
    profileNames <- basename(pathtofiles) %>% 
      tools::file_path_sans_ext()
  } else {
    profileNames <- profileNames
  }
  raw_profiles <- lapply(pathtofiles, read_single_exposure)
  names(raw_profiles) <- profileNames

  # any incorrect profiles?
  flagCorrect <- do.call(c, lapply(raw_profiles,function(x) length(x)!=0))
  raw_profiles <- raw_profiles[flagCorrect]
  
  
  return(raw_profiles)
}

#' Check exposure profile
#'
#' @param p the exposure profile 
#'
#' @return the profile or NULL if not correct
#' @noRd
check_profile <- function(p){
  # check if has content
  if (!length(p)){
    out <- "File is empty"
    warning(out)
    return(NULL)
  }
  
  
  
  # check if two or more columns
  if (ncol(p) < 2){
    out <- "File is not a plain-text file with two or more columns."
    warning(out)
    return(NULL)
  }
  
  # check if necessary columns are available
  if (!all( c("time", "conc") %in% colnames(p) )){
    out <- "Not all necessary columns ('time' and 'conc') available."
    warning(out)
    return(NULL)
  }
  
  # check if columns give numeric data
  colClasses <- c(class(p[,'time']), class(p[,'conc']))
  acceptedClass <- !any(!(colClasses %in% c("numeric", "integer")))
  if (!acceptedClass){
    out <- "The columns are neither 'numeric' nor 'integers'."
    warning(out)
    return(NULL)
  }
  
  
  # check if the time column is increasing
  # timeCol <- p[,'t']
  # increasing <- !any(diff(timeCol)<=0)
  # if (!increasing){
  #   out <- "Time is not increasing."
  #   warning(out)
  #   return(NULL)
  # }
  
  out <- p
  return(out)
  
}
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
  out <- p
  return(out)
  
}


#' Function to read TOXSWA exposure profiles from out-files
#' 
#' Read several expsoure profiles at their corresponding pathes at 'pathtofiles'
#'
#' @param pathtofiles path to toxswa files that should be read
#' @param profileNames a vector of names to be used for the loaded toxswa files. if "NA" the filename without extension will be used
#'
#' @export
#'
#' @return list of exposure profiles each with two columns (time and concentration)
readToxswaExpProfiles <- function(pathtofiles, profileNames=NA){
  if ( any(sapply(profileNames, is.na)) ) {
    profileNames <- basename(pathtofiles) %>% 
      tools::file_path_sans_ext()
  } else {
    profileNames <- profileNames
  }
  profiles <- lapply(pathtofiles, extract_focus_profile)
  names(profiles) <- profileNames
  
  flag_null <- do.call(c, lapply(profiles, is.null))
  profiles <- profiles[!flag_null]
  
  profiles <- lapply(setNames(names(profiles), names(profiles)),
                     function(x){
                       split_toxswa_exposure_profile(profiles[[x]], x)
                     })
  profiles <- flatten_list(profiles)
  
  profiles <- lapply(profiles, melt_Focus_profiles)

  #trialnames <- rep_len(profileNames,2)
  profiles <- lapply(setNames(names(profiles),names(profiles)), function(i){
    profiles[[i]]["trial"] <- i
    profiles[[i]]
  })

  return(profiles)
}

#' Extract a focus profile from a single .out file
#'
#' @param file_name 
#' @param layer_string 
#' @param substance_string 
#'
#' @return exposure profiles
#' @noRd
extract_focus_profile <- function(
    file_name,
    layer_string = "ConLiqWatLay",
    substance_string = ""
) {
  
  lin <- readLines(file_name, encoding = "UTF-8")
  
  isToxswa <- check_if_TOXSWA(lin)
  if ( !isToxswa ){
    message("file is not a toxswa file")
    return(NULL)
  }
  
  is_supported_version <- check_if_TOXSWA_version(lin, supported_version = c("4","5.5.3"))
  if ( !is_supported_version ){
    message(paste0("Toxswa version is not supported. So far only version 4 and 5.5.3 are supported."))
    return(NULL)
  }
  
  unit <- lin %>%
    stringr::str_subset(
      paste0("\\* Unit for ", head(stringr::str_split(layer_string, "_")[[1]],1), " is")
    ) %>%
    stringr::str_split("[\\(\\)]") %>%
    unlist() %>%
    magrittr::extract(2) %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("\\.", " ")
  
  lin %>%
    stringr::str_subset(stringr::str_c(layer_string, "_", substance_string)) %>%
    stringr::str_split(" +") %>%
    sapply(function(x) c(x[3:4], tail(x,1))) %>%
    t() %>%
    dplyr::as_tibble(.name_repair = ~paste0("V", seq_along(.))) %>%
    dplyr::transmute(
      Layer_Specifier = as.character(V2),
      DateTime = as.character(V1) %>% lubridate::dmy_hm(),
      `Time` = units::set_units((DateTime - DateTime[1]) / lubridate::ddays(1), d),
      Exposure = units::set_units(as.numeric(V3), unit, mode = "standard")
    ) %>%
    tibble::add_column(
      RunID =
        stringr::str_split(file_name, "[/\\\\]")[[1]] %>%
        tail(1) %>%
        stringr::str_extract("[[:digit:]]+"),
      .before = "DateTime"
    ) %>%
    dplyr::select(RunID, Layer_Specifier, DateTime, `Time`, Exposure)  %>%
    dplyr::rename_at("Exposure", ~ paste0("Concentration"))
}

#' Split a profile into a list if more then one active substances was detected
#'
#' @param profile the toxswa profile as a tibble
#' 
#' @export
#'
#' @return a tibble or a list of tibbles if more than one active substance was identified
#'
#' @examples
#' dat <- data.frame(RunID = 123, 
#'   Layer_Specifier = rep(c("ConLiqWatLay_ai1","ConLiqWatLay_ai2"),10),
#'   Concentration = rlnorm(20,0,1)
#'   )
#'   split_toxswa_exposure_profile(dat)
split_toxswa_exposure_profile <- function(profile, runid=NA){
  
  # ai holds the name of the active ingredient for each point in the time series
  ai <- sub("^ConLiqWatLay_(.+)$","\\1",profile[["Layer_Specifier"]])
  
  if (length(unique(ai)) == 1 ){ # if only one ai return the profile
    return(profile)
    
  } else if (length(unique(ai)) > 1){ # if more than one split the time series in a list
    if (is.na(runid)){
      runid_ <- profile[1,"RunID"]
    } else {
      runid_ <- runid
    }
    out <- split(profile, profile[,"Layer_Specifier"])
    names(out) <- paste0(runid_,"_",unique(ai))
    return(out)
    
  } else {
    return(NULL)
  }
  
}


#' Checks if the input contains the default TOXSWA keywords in the header
#'
#' @param ln 
#'
#' @return TRUE if header matches the default TOXSWA header
#' @noRd
#' @examples
#'  ln <- c("TOXSWA REPORT: Header",
#'  "* Results from the TOXSWA model  (c) Alterra",
#'  "* FOCUS  TOXSWA version   : 4",
#'  "* TOXSWA model version    : 3.3.4",
#'  "* TOXSWA created on       : 02-Apr-2015")
#'  check_if_TOXSWA(ln)
check_if_TOXSWA <- function(ln){
  ln <- ln[1:20]
  strings_to_match <- c(
    "^\\* Results from the TOXSWA model\\s*\\(c\\)",
    "^\\* FOCUS\\s*TOXSWA version\\s*:",
    "^\\* TOXSWA model version\\s*:",
    "^\\* TOXSWA created on\\s*:"
  )
  
  contains_strings <- do.call(c,lapply(strings_to_match, function(x) any(grepl(pattern=x, ln))))
  
  !any(!contains_strings)
  
}


#' Reduce the focus exposure profile
#' 
#' Reduce to a two column data.frame with first column indicating the time
#' and the second column the concentration
#'
#' @param profile 
#' @param time_col 
#' @param conc_col 
#'
#' @return the reduced FOCUS profile
#' @noRd
melt_Focus_profiles <- function(profile, time_col = "Time", conc_col =  "Concentration"){
  out <- profile %>% 
    dplyr::select(c(time_col,conc_col)) %>%
    units::drop_units() %>% 
    as.data.frame()
  
  names(out) <- c("time", "conc")

  return(out)
}


#' Checks if the toxswa version is as specified
#'
#' @param ln a vector of strings
#' @param supported_version a string of the version that is looked for
#'
#' @return TRUE if version numbers match
#' 
#' @export
#'
#' @examples
#' ln <- c("* ------------------------------------------------------------------------------",
#' "* Results from the TOXSWA model  (c) Wageningen University & Research",
#' "* FOCUS  TOXSWA version   : 5.5.3",
#' "* TOXSWA model version    : 3.3.6",
#' "* TOXSWA created on       : 17-Dec-2017")
#' check_if_TOXSWA_version(ln, supported_version = "5.5.3")
#' 
#'  ln2 <- c("TOXSWA REPORT: Header",
#'  "* Results from the TOXSWA model  (c) Alterra",
#'  "* FOCUS  TOXSWA version   : 4",
#'  "* TOXSWA model version    : 3.3.4",
#'  "* TOXSWA created on       : 02-Apr-2015")
#' check_if_TOXSWA_version(ln2)
check_if_TOXSWA_version <- function(ln, supported_version = c("4", "5.5.3") ){
  regex_version <- "^\\*\\s+FOCUS\\s+TOXSWA\\s+version"
  Toxswa_version_line <- grep(regex_version, ln, value = TRUE)
  Toxswa_version <- sub(paste0(regex_version,"\\s+:\\s*(.*)$"),"\\1", Toxswa_version_line)
  Toxswa_version %in% supported_version
}
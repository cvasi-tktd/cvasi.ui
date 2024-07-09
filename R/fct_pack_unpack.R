
#' Serialize a list to JSON/zip
#'
#' @param zipfile path to output zipfile
#' @param x the list that is serialized
#'
#' @return path to the zip file
pack <- function(x, zipfile){
  
  # create json string
  json <- jsonlite::toJSON(x, 
                           auto_unbox = TRUE,
                           digits = 8, 
                           pretty = TRUE, 
                           na = "null")
  
  # write json to file
  tempf <- file.path(tempdir(), "input.json")
  fp <- file(tempf, "w")
  cat(json, file = fp, sep = "\n")
  close(fp)
  
  # add json to zip
  zip::zip(zipfile, files = tempf, mode = "cherry-pick")
}


#' Deserialize a list to JSON/zip
#'
#' @param zipfile path to output zipfile
#'
#' @return the content of the zipped json file as list
unpack <- function(zipfile){
  if(!file.exists(zipfile))
    stop("zipfile is missing: ", zipfile)
  
  # unzip to temp dir
  zip::unzip(zipfile)
  
  # import json file at temp dir 
  json <- paste0(tempdir(),"/input.json")
  jsonlite::fromJSON(json, na = "string", simplifyVector = FALSE)
  
}
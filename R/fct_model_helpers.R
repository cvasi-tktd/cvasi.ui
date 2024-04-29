#' Call constructor of model
#' 
#' Call the constructor of a model by its name as "string" 
#' 
#' 
#' @param model_name the model name as string
#'
#' @return the corresponding model object
#'
#' @examples
#' \dontrun{
#' construct_model("GUTS_RED_IT")
#' }
construct_model <- function(model_name){
  f <- get(model_name)
  x <- f()
  stopifnot(inherits(x, "EffectScenario"))
  
  return(x)
}



#' Get the required parameter of init names of a model
#'
#' @param x the effect model object
#' @param type the type to check; either "param", "init" or forcing
#'
#' @return a vector with the names of the required parameters, initial values or forcings
#'
#' @examples
#' \dontrun{
#' dat <- cvasi::GUTS_RED_IT()
#' get_required(dat, type = "param")
#' get_required(dat, type = "init")
#' get_required(dat, type = "forcings")
#' }
get_required <- function(x, type = c("param", "init", "forcings")){
  stopifnot(inherits(x, "EffectScenario"))
  type <- match.arg(type)
  out <- switch(type,
                "param" = x@param.req,
                "init" = names(x@init),
                "forcings" = x@forcings.req
                )
  return( out )
}


#' Get parameters, forcings or initial values of a 
#'
#' @param x the effect model object
#' @param type the type to check; either "param", "init" or forcings
#'
#' @return list of parameters, initial values or forcings
#'
#' @examples
#' \dontrun{
#' dat <- cvasi::Lemna_Schmitt()
#' get_val(dat, type = "param")
#' get_val(dat, type = "init")
#' get_val(dat, type = "forcings")
#' }
get_val <- function(x, type = c("param", "init", "forcings")){
  stopifnot(inherits(x, "EffectScenario"))
  type <- match.arg(type)
  out <- switch(type,
                "param" = x@param,
                "init" = x@init,
                "forcings" = x@forcings
  )
  return( out )
}

#' Does the model require forcings?
#'
#' @param x the effect model object
#'
#' @return TRUE if forcings are required, FALSE otherwise
#'
#' @examples
#' \dontrun{
#' forcings_required(cvasi::metsulfuron)
#' }
forcings_required <- function(x){
  stopifnot(inherits(x, "EffectScenario"))
  length(x@forcings.req)>0
}



#' Checks if the model parameters, forcings or initial values are complete
#'
#' @param x the effect model object
#' @param type the type to check; either "param", "init" or forcings
#'
#' @return TRUE if model complete, FALSE otherwise
#'
#' @examples
#' \dontrun{
#' check_model_complete(cvasi::metsulfuron, type = "param")
#' check_model_complete(cvasi::metsulfuron, type = "init")
#' check_model_complete(cvasi::metsulfuron, type = "forcing")
#' 
#' check_model_complete(cvasi::GUTS_RED_SD(), type = "forcings")
#' }
check_model_complete <- function(x, 
                                 type = "param"){
  stopifnot(inherits(x, "EffectScenario"))
  
  supported_type <- c("param", "init", "forcings")
  if (!(type %in% supported_type)){
    stop("Chosen type not supported.")
  }
  
  if (type == "forcings" && !forcings_required(x)){
    warning("Forcing not required. Returning 'TRUE'")
    return(TRUE)
  }
  
  
  all_req <- get_required(x, type)
  val <- get_val(x,type)
  
  # names complete?
  if (length(names(val))>0){
    names_complete <- all(names(val) %in% all_req)  
  } else {
    names_complete <- FALSE
  }
  
  
  # any value non-NULL/non-NA?
  has_value <- !(any(is.na(val)) | any(is.null(val)))
  
  if (names_complete && has_value){
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}

#' Checks if the exposure data is complete
#'
#' @param x a data.frame for the exposure
#'
#' @return TRUE if exposure data is complete, FALSE otherwise
#'
#' @examples
#' \dontrun{
#' exp_dat <- cvasi::Schmitt2013 |>
#'                dplyr::select(t, ID, conc) |>
#'                dplyr::rename(trial = "ID", time = "t")
#' check_exposure_complete(exp_dat)
#' }
check_exposure_complete <- function(x){
  expected_colnames <- c("time", "trial", "conc")
  x_exist <- length(x) > 0
  
  if (x_exist){
    all_cols_exist <- all(expected_colnames %in% colnames(x))
  } else {
    all_cols_exist <- FALSE
  }
  
  if (x_exist && all_cols_exist){
    return(TRUE)
  }else{
    return(FALSE)
  }
  
  
  
}


#' Checks if the forcing data is complete
#'
#' @param expected_forcings a character vector listing the expected forcings
#' @param forcings a character vector listing the available forcings
#'
#' @return TRUE if forcings are complete, FALSE otherwise
check_forcings_complete <- function(expected_forcings, forcings){
  if (length(expected_forcings) != length(forcings)){
    return(FALSE)
  } 
  
  if ( all(expected_forcings %in% names(forcings)) ) {
    return(TRUE)
  }
}


#' Lookup model/scenario/contructor name
#'
#' @param x the string to lookup
#' @param lookup_table the lookup table to use
#' @param from find x in column "from"
#' @param to get value in column "to
#'
#' @return a string with the found value in column "to"
#' @examples
#' \dontrun{
#' "LemnaSchmittScenario" %>% 
#'   lookup_name(model_lookup,
#'             from = "scenario",
#'             to = "model_f")
#'}
lookup_name <- function(x, 
                        lookup_table = model_lookup,
                        from = "scenario", 
                        to = "model_f"){
  stopifnot(from %in% colnames(lookup_table))
  stopifnot(to %in% colnames(lookup_table))
  
  lookup_table %>% 
    dplyr::filter(!!as.symbol(from) == !!x ) %>% 
    dplyr::pull(to)
}
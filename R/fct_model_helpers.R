#' Call constructor of model
#' 
#' Call the constructor of a model by its name as "string" 
#' 
#' 
#' @param model_name 
#'
#' @return
#' @export
#'
#' @examples
#' construct_model("GUTS_RED_IT")
construct_model <- function(model_name){
  f <- get(model_name)
  f()
}



#' Get the required parameter of init names of a model
#'
#' @param x the effect model object
#' @param type the type to check; either "param", "init" or forcing
#'
#' @return
#' @export
#'
#' @examples
#' dat <- neofm::GUTS_RED_IT()
#' get_required(dat, type = "param")
#' get_required(dat, type = "init")
#' get_required(dat, type = "forcings")
get_required <- function(x, type){
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
#' @return
#' @export
#'
#' @examples
#' dat <- neofm::GUTS_RED_IT()
#' get_val(dat, type = "param")
#' get_val(dat, type = "init")
#' get_val(dat, type = "forcings")
get_val <- function(x, type){
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
#' @return
#' @export
#'
#' @examples
#' forcings_required(neofm::metsulfuron)
forcings_required <- function(x){
  length(x@forcings.req)>0
}



#' Checks if the model parameters, forcings or initial values are complete
#'
#' @param x the effect model object
#' @param type the type to check; either "param", "init" or forcings
#'
#' @return
#' @export
#'
#' @examples
#' check_model_complete(neofm::metsulfuron, type = "param")
#' check_model_complete(neofm::metsulfuron, type = "init")
#' check_model_complete(neofm::metsulfuron, type = "forcing")
#' 
#' check_model_complete(neofm::GUTS_RED_SD(), type = "forcings")
check_model_complete <- function(x, 
                                 type = "param"){
  supported_type <- c("param", "init", "forcings")
  if (!(type %in% supported_type)){
    stop("Chosen type not supported.")
  }
  
  if (type == "forcing" & !forcings_required(x)){
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
  
  if (names_complete & has_value){
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}

#' Checks if the exposure data is complete
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
#' exp_dat <- neofm::Schmitt2013 |>
#'                dplyr::select(t, ID, conc) |>
#'                dplyr::rename(trial = "ID", time = "t")
#' check_exposure_complete(exp_dat)
check_exposure_complete <- function(x){
  expected_colnames <- c("time", "trial", "conc")
  
  x_exist <- length(x) > 0
  if (x_exist){
    all_cols_exist <- all(expected_colnames  %in% colnames(x))
  } else {
    all_cols_exist <- FALSE
  }
  
  if (x_exist & all_cols_exist){
    return(TRUE)
  }else{
    return(FALSE)
  }
  
  
  
}


check_forcings_complete <- function(expected_forcings, forcings){
  if (length(expected_forcings) != length(forcings)){
    return(FALSE)
  } 
  
  if ( all(expected_forcings %in% names(forcings)) ) {
    return(TRUE)
  }
}


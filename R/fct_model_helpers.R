#' Call constructor of model
#' 
#' Callc the constructor of a model by its name as "string" 
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
#' @param type the required type to get; either "param" or "init"
#'
#' @return
#' @export
#'
#' @examples
#' dat <- neofm::GUTS_RED_IT()
#' get_required(dat, type = "param")
#' get_required(dat, type = "init")
get_required <- function(x, type){
  out <- switch(type,
                "param"= x@param.req,
                "init" = names(x@init)
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
#' forcings_required(metsulfuron)
forcings_required <- function(x){
  length(x@forcings.req)>0
}

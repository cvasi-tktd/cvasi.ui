

#' create an div container with title
#'
#' @param mytext 
#' @param tooltip 
#'
#' @return
#' @export
#'
#' @examples
#' tooltip_text(mytext = "Tmin", 
#'              tooltip = get_parameter_info(
#'                           model_ = "Lemna_Schmitt", 
#'                           parameter_ = "Tmin", 
#'                           type_ ="description"
#'                           )
#'              )
#' tooltip_text(mytext = "Tmin")
tooltip_text <- function(mytext, tooltip){
  if (missing(tooltip)){
    out <- paste0("<div>", mytext, "</div>")
  } else {
    out <- paste0("<div title='",tooltip,"'>", mytext, "</div>")
  }
  
  return(HTML(out))
}

#' Generate label for input fields with unit
#'
#' @param name the parameter/initial value name
#' @param unit the unit of the parameter/initial value
#'
#' @return a string of the parameter/initial value name and its unit
field_label <- function(name, unit){
  unit_out <- dplyr::case_when(unit == "logical" ~ "[-]",
                               is.na(unit) ~ "",
                               unit == "" ~ "",
                               .default = paste0("[",unit,"]")
  )
  
  trimws(paste0(name, " ", unit_out))
}

#' create an div container with title
#'
#' @param mytext the text to be displayed
#' @param tooltip The text that appears when the mouse hovers over `mytext`
#'
#' @return an html div-container
#' 
#' @examples
#' \dontrun{
#' tooltip_text(mytext = "Tmin", 
#'              tooltip = get_parameter_info(
#'                           model_ = "Lemna_Schmitt", 
#'                           parameter_ = "Tmin", 
#'                           type_ ="description"
#'                           )
#'              )
#' tooltip_text(mytext = "Tmin")
#' }
tooltip_text <- function(mytext, tooltip){
  if (missing(tooltip)){
    out <- paste0("<div>", mytext, "</div>")
  } else {
    out <- paste0("<div title='",tooltip,"'>", mytext, "</div>")
  }
  
  return(HTML(out))
}

#' Add info-icon to text
#'
#' @param mytext the main text after which the info button will appear
#' @param button_id the id of info button that can be referenced by e.g. observeEvent
#' 
#' @return html object
#' @examples
#' \dontrun{
#' mytext <- "Parameters"
#' infoButtonId <- "boxinfo"
#' add_info_icon(mytext, infoButtonId)
#' }
add_info_icon <- function(mytext, button_id){
  
  id <- sub("^(.*-).*$","\\1",button_id)
  out <- htmltools::HTML(
    mytext,
    as.character(
      actionLink(inputId = button_id,
                 label = "",
                 icon = icon("info-circle"),
                 class = "info-circle-link")
    )
  )
  
  
  return(out)
}

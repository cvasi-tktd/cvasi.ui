#' fileInputOnlyButton
#'
#' A simplified version of the shiny::fileInputButton.
#' Use in the same way as you would use shiny::fileInputButton.
#'
#' @param ... 
#' @param label 
#'
#' @return a file input button DOM
#' @noRd
fileInputOnlyButton <- function(..., label="") {
  temp <- fileInput(..., label=label)
  # Cut away the label
  temp$children[[1]] <- NULL
  # Cut away the input field (after label is cut, this is position 1 now)
  temp$children[[1]]$children[[2]] <- NULL
  # Remove input group classes (makes button flat on one side)
  temp$children[[1]]$attribs$class <- NULL
  temp$children[[1]]$children[[1]]$attribs$class <- NULL
  temp
}

#' Create a dashboard box collapsible on click on title
#'
#' @param ... arguments passed to `shinydashboard::box()`
#'
#' @return
dashboardbox_left <- function(...){
  x <- shinydashboard::box(...)
  f_args <- list(...)
  if (length(f_args)){
    if (all(c("title", "collapsible") %in% names(f_args))){
      if (f_args[["collapsible"]]){
        box_title <- x$children[[1]]$children[[1]]$children[[1]]
        x$children[[1]]$children[[1]]$children <- x$children[[1]]$children[[1]]$children[[2]]$children
        x$children[[1]]$children[[1]]$children[[1]]$children[[2]] <- box_title
      }
    }
  }
  return(x)
}

#' Set locale of input container
#'
#' @param x result of `shiny::numericInput()`
#' @param locale locale as string, e.g. "de-DE" for German locale
set_lang <- function(x, locale = "en-US"){
  stopifnot(class(x) == "shiny.tag")
  stopifnot(x$attribs$class == "form-group shiny-input-container")
  x$children[[2]]$attribs$lang <- locale  
  return(x)
}


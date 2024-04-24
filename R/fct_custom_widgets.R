#' Function to create value boxes
#' 
#' Create based on the value (logical) a green thumbs-up box if `TRUE`, or a red 
#' thumbs-down box, if `FALSE`.
#' 
#' @param value to display in big bold letters
#' @param subtitle to display in smaller letter below 'value'
#' @param width width of the box
#' 
#' @return a shinydashboard value box
create_valuebox <- function(value, subtitle, width = 3) {
  
  output_val <- switch(as.character(value),
                       `TRUE` = "Ok",
                       `FALSE` = "Missing",
                       `NA` = "minus")
  
  icon_char <- switch(as.character(value),
                      `TRUE` = "thumbs-up",
                      `FALSE` = "thumbs-down",
                      `NA` = "minus")
  
  color_char <- switch(as.character(value),
                       `TRUE` = "green",
                       `FALSE` = "red",
                       `NA` = "yellow")
  
  valueBox(
    value = output_val,
    subtitle = subtitle,
    icon = icon(icon_char),
    color = color_char,
    width = width
  )
}


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
#' @return a shinydasboard box with collapse icon on the left side
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


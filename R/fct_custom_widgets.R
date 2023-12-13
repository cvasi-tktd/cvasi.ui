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

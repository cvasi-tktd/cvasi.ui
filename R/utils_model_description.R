#' Convert to md to html
#'
#' @param x a string or a vector of strings
#'
#' @return html character
#'
#' @examples
#' s <- c("This ", "is not ", "a test")
#' toHTML(s)
toHTML <- function(x){
  htmltools::HTML(commonmark::markdown_html(paste(x, collapse="")))
}

#' Read roxygen help
#'
#' @param package the name of the package
#' @param f_name the name of the function you want to find
#' @param tag the tag of the paragraph you want to extract
#' @param print_tags_only if TRUE the available tags are printed
#'
#' @return a string of the content of the respective roxygen header tag
#'
#' @examples
#' read_roxygen("base", "data.frame", "\\details")
read_roxygen <- function(package, f_name, tag, print_tags_only = FALSE){
  e <- new.env(parent = emptyenv())
  f <- paste0(system.file("help/",package = package),"/",package)
  
  lazyLoad(f, envir = e)
  help_txt <- get(f_name, envir = e)
  
  tags <- tools:::RdTags(help_txt)
  if (print_tags_only) return(tags)
  flag_tags <- which(tags == tag)
  extr_text <- unlist(help_txt[flag_tags])
  
  toHTML(extr_text)
}
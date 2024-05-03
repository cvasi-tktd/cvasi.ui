#' Convert to md to html
#'
#' @param x a string or a vector of strings
#'
#' @return html character
#'
#' @examples
#' \dontrun{
#' s <- c("This ", "is not ", "a test")
#' toHTML(s)
#' }
toHTML <- function(x){
  stopifnot(is.character(x))
  htmltools::HTML(commonmark::markdown_html(paste(x, collapse="")))
}

#' Read roxygen help
#'
#' @param package the name of the package
#' @param f_name the name of the function you want to find
#' @param tag the tag of the paragraph you want to extract
#' @param print_tags_only if TRUE the available tags are printed
#'
#' @return a string of the content of the respective roxygen header tag or a vector with available tags of a function
#'
#' @examples
#' \dontrun{
#' read_roxygen(package = "base", f_name = "data.frame", tag = "\\details")
#' }
read_roxygen <- function(package, f_name, tag, print_tags_only = FALSE){
  e <- new.env(parent = emptyenv())
  stopifnot(require(package, character.only = TRUE))
  
  f <- paste0(system.file("help/",package = package),"/",package)
  
  lazyLoad(f, envir = e)
  help_txt <- get(f_name, envir = e)
  
  tags <- RdTags(help_txt)
  if (print_tags_only) return(tags)
  stopifnot(!missing(tag))
  flag_tags <- which(tags == tag)
  extr_text <- unlist(help_txt[flag_tags])
  
  toHTML(extr_text)
}

#' RdTags
#' local implementation of tools:::RdTags
#' @noRd
RdTags <- function (Rd)
{
  res <- lapply(Rd, attr, "Rd_tag")
  if (length(res))
    simplify2array(res, FALSE)
  else character()
}

#' Extension of purrr::list_rbind
#' 
#' When list is bind the names of the list elements are added as additional column
#'
#' @param x 
#' @param listnames_to 
#' @param ... 
#' @param names_to 
#' @param ptype 
#'
#' @return
#' @export
#'
#' @examples
#' x1 <- list(a = data.frame(time=1:3, value = rnorm(3)),
#'            b = data.frame(time=1:3, value = rnorm(3)),
#'            c = data.frame(time=1:3, value = rnorm(3)))
#' list_rbind2(x1, listnames_to = "my_factor")
list_rbind2 <- function(x, listnames_to = "listname",... , names_to = rlang::zap(), ptype = NULL){
  if ( !(class(x) %in% "list")) stop("x should be a list!")
  
  listnames <- setNames(names(x),names(x))
  lst <- lapply(listnames, function(i){
    x[[i]] %>% dplyr::mutate( "{listnames_to}" := i)
  })
  
  purrr::list_rbind(lst)
}


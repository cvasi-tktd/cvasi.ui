#' Extension of purrr::list_rbind
#' 
#' When list is bind, the names of the list elements are added as additional column
#'
#' @param x a list with data.frame elements
#' @param listnames_to the name of the column in the returned data.frame that contains the list names
#' @param ... passed to `purrr::list_rbind`
#'
#' @return a data.frame that is merged from the data.frames of the list 
#' @importFrom rlang `:=`
#' @importFrom stats setNames
#' 
#' @examples
#' \dontrun{
#' x1 <- list(a = data.frame(time=1:3, value = rnorm(3)),
#'            b = data.frame(time=1:3, value = rnorm(3)),
#'            c = data.frame(time=1:3, value = rnorm(3)))
#' list_rbind2(x1, listnames_to = "my_factor")
#' # compare with 
#' list(rbind(x1))
#' }
list_rbind2 <- function(x, listnames_to = "listname", ...){
  if ( !(class(x) %in% "list")) stop("x should be a list!")
  stopifnot(is_named_list(x))
  
  listnames <- setNames(names(x),names(x))
  lst <- lapply(listnames, function(i){
    x[[i]] %>% dplyr::mutate( "{listnames_to}" := i)
  })
  
  purrr::list_rbind(lst, ...)
}


#' Validate if list is named
#'
#' @param lst the list to validate
#'
#' @return TRUE if the list is named; FALSE otherwise
#' @examples
#' \dontrun{
#' x1 <- list(a = 1:10, b = 2:5, c = 3)
#' is_named_list(x1) # TRUE
#' 
#' x2 <- list(1:10, 2:5, 3)
#' is_named_list(x2) # FALSE
#' 
#' x3 <- list(a= 1:10, 2:5, 3)
#' is_named_list(x3) # FALSE
#' }
is_named_list <- function(lst){
  stopifnot(class(lst) == "list")
  length(lst) == sum(names(lst) != "",na.rm = TRUE)
}

#' Create a list of grouped parameters
#'
#' @param p a character vector listing the parameter names
#' @param model_ the model for which the parameters should be grouped
#'
#' @return a list with parameters seperated into groups as specified by cvasi.ui::parameter_descriptions
#'
#' @seealso [cvasi.ui::parameter_descriptions]
#' @global group parameter
#' @examples
#' \dontrun{
#' group_parameters(p = c("k_phot_fix", "k_phot_max", "k_resp", "k_loss", "Tmin", "Tmax", 
#'                        "Topt", "t_ref", "Q10", "k_0", "a_k", "C_P", "CP50", "a_P", "KiP", 
#'                        "C_N", "CN50", "a_N", "KiN", "BM50", "mass_per_frond", "BMw2BMd", 
#'                        "Emax", "EC50", "b", "P_up", "AperBM", "Kbm", "P_Temp", "MolWeight"
#'                        ),
#'                  model_ = "Lemna_Schmitt")
#'}
group_parameters <- function(p, model_){
  stopifnot(is.character(p))
  stopifnot(is.character(model_))
  stopifnot(length(model_) == 1)
  p_groups <- cvasi.ui::parameter_descriptions %>% 
    dplyr::filter(model == model_, parameter %in% p) %>% 
    dplyr::select(parameter, group)
  
  out <- split(p_groups[,"parameter"], p_groups[,"group"]) %>% 
    order_parameter_groups(c("toxicodynamic", "toxicokinetic", "physiological"))
    
  
  return(out)
}

#' Seperate parameters into expert or non-expert groups
#'
#' @param p a character vector listing the parameter names
#' @param model_  the model for which the parameters should be grouped into expert and non-expert
#'
#' @return  a list with parameters seperated into expert and non-expert groups as specified by cvasi.ui::parameter_descriptions
#'
#' @seealso [cvasi.ui::parameter_descriptions]
#' @global model parameter expert.value
#' @examples
#' \dontrun{
#' expert_parameters(p = c("k_phot_fix", "k_phot_max", "k_resp", "k_loss", "Tmin", "Tmax", 
#'                        "Topt", "t_ref", "Q10", "k_0", "a_k", "C_P", "CP50", "a_P", "KiP", 
#'                        "C_N", "CN50", "a_N", "KiN", "BM50", "mass_per_frond", "BMw2BMd", 
#'                        "Emax", "EC50", "b", "P_up", "AperBM", "Kbm", "P_Temp", "MolWeight"
#'                        ),
#'                  model_ = "Lemna_Schmitt")
#'}
expert_parameters <- function(p, model_){
  stopifnot(is.character(p))
  stopifnot(is.character(model_))
  stopifnot(length(model_) == 1)
  p_expert <- cvasi.ui::parameter_descriptions %>% 
    dplyr::filter(model == model_, parameter %in% p) %>% 
    dplyr::select(parameter, expert.value) 
  
  out <- split(p_expert[,"parameter"], p_expert[,"expert.value"])
  
  return(out)
}


#' Create a title with icon for the thematic parameter groups
#'
#' @param group_title a string denoting the parameter group
#'
#' @return a shiny.tag object with specified icon or if group not defined the input string is returned unmodified
group_title_with_icon <- function(group_title){
  stopifnot(is.character(group_title))
  stopifnot(length(group_title) == 1)
  switch(group_title,
         "physiological" = span(icon("heartbeat"), "physiological"),
         "toxicokinetic" = span(icon("exchange"), "toxicokinetic"),
         "toxicodynamic" = span(icon("bolt"),"toxicodynamic"),
         group_title
         )
}

#' Order parameter groups
#'
#' @param x a list with grouped parameter names; result from `group_parameters`
#' @param group_order a character vector indicating the order of parameter groups
#'
#' @return `x` ordered by `group_order`
#' @examples
#' \dontrun{
#' parameter_groups <- list(
#' physiological = c("k_phot_fix", "k_phot_max", "k_resp", 
#'    "k_loss", "Tmin", "Tmax", "Topt", "t_ref", "Q10", "k_0", "a_k", 
#'    "C_P", "CP50", "a_P", "KiP", "C_N", "CN50", "a_N", "KiN", "BM50", 
#'    "mass_per_frond", "BMw2BMd"), 
#'toxicodynamic = c("Emax", "EC50", "b"), 
#'toxicokinetic = c("P_up", "AperBM", "Kbm", "P_Temp", "MolWeight"))
#'
#' parameter_groups
#' order_parameter_groups(parameter_groups, c("toxicodynamic", "toxicokinetic", "physiological"))
#' }
order_parameter_groups <- function(x, group_order){
  x_names <- names(x)
  
  x[order(match(x_names, group_order))]
  
}

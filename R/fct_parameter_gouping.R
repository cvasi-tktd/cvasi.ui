
#' Create a list of grouped parameters
#'
#' @param p 
#' @param model_ 
#'
#' @return
#' @export
#'
#' @examples
#' group_parameters(p = c("k_phot_fix", "k_phot_max", "k_resp", "k_loss", "Tmin", "Tmax", 
#'                        "Topt", "t_ref", "Q10", "k_0", "a_k", "C_P", "CP50", "a_P", "KiP", 
#'                        "C_N", "CN50", "a_N", "KiN", "BM50", "mass_per_frond", "BMw2BMd", 
#'                        "Emax", "EC50", "b", "P_up", "AperBM", "Kbm", "P_Temp", "MolWeight"
#'                        ),
#'                  model_ = "Lemna_Schmitt")
group_parameters <- function(p, model_){
  p_groups <- cvasiUI::parameter_descriptions %>% 
    dplyr::filter(model == model_, parameter %in% p) %>% 
    dplyr::select(parameter, group) 
  
  out <- split(p_groups[,"parameter"], p_groups[,"group"])
  
  return(out)
}

#' Seperate parameters into expert or non-expert groups
#'
#' @param p 
#' @param model_ 
#'
#' @return
#' @export
#'
#' @examples
#' expert_parameters(p = c("k_phot_fix", "k_phot_max", "k_resp", "k_loss", "Tmin", "Tmax", 
#'                        "Topt", "t_ref", "Q10", "k_0", "a_k", "C_P", "CP50", "a_P", "KiP", 
#'                        "C_N", "CN50", "a_N", "KiN", "BM50", "mass_per_frond", "BMw2BMd", 
#'                        "Emax", "EC50", "b", "P_up", "AperBM", "Kbm", "P_Temp", "MolWeight"
#'                        ),
#'                  model_ = "Lemna_Schmitt")
expert_parameters <- function(p, model_){
  p_expert <- cvasiUI::parameter_descriptions %>% 
    dplyr::filter(model == model_, parameter %in% p) %>% 
    dplyr::select(parameter, expert.value) 
  
  out <- split(p_expert[,"parameter"], p_expert[,"expert.value"])
  
  return(out)
}


#' Create a title with icon for the thematic parameter groups
#'
#' @param group_title 
#'
#' @return
#' @export
group_title_with_icon <- function(group_title){
  switch(group_title,
         "physiological" = span(icon("heartbeat"), "physiological"),
         "toxicokinetic" = span(icon("exchange"), "toxicokinetic"),
         "toxicodynamic" = span(icon("bolt"),"toxicodynamic"),
         group_title
         )
}

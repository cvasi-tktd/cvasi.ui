
#' get info from parameter_descriptions data
#'
#' @param model 
#' @param parameter 
#' @param type one of "description", "unit", "group", "expert.value", "default"
#'
#' @return
#' @export
#'
#' @examples
#' get_parameter_info(model_ = "Lemna_Schmitt", parameter_ = "Tmin", type_ ="description")
#' get_parameter_info(model_ = "Lemna_Schmitt", parameter_ = "Tmin", type_ ="unit")
#' get_parameter_info(model_ = "Lemna_Schmitt", parameter_ = "Tmin", type_ ="group")
#' get_parameter_info(model_ = "Lemna_Schmitt", parameter_ = "Tmin", type_ ="expert.value")
#' get_parameter_info(model_ = "Lemna_Schmitt", parameter_ = "Tmin", type_ ="default")
get_parameter_info <- function(model_, parameter_, type_){
  if (!(type_ %in% colnames(parameter_descriptions))){
    warning(paste0(type_," is not available in 'parameter_descriptions'."))
    return(NULL)
  }
  
  mod_par <- parameter_descriptions %>% 
    dplyr::filter(model == model_, parameter == parameter_)
  
  if (!nrow(mod_par)){
    warning(paste0("Parameter ",parameter_," is not available for model ",model_," in 'parameter_descriptions'."))
    return(NULL)
  }
  
  mod_par %>% 
    dplyr::select(matches(type_))
}



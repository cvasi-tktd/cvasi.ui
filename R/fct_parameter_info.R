
#' get info from parameter_descriptions data
#'
#' @param model_ the model as defined in cvasi.ui::parameter_descriptions
#' @param parameter_ the parameter as defined in cvasi.ui::parameter_descriptions
#' @param type_ one of "description", "unit", "group", "expert.value", "default"
#'
#' @return a string for the model and parameter and column type
#' 
#' @examples
#' \dontrun{
#' get_parameter_info(model_ = "Lemna_Schmitt", parameter_ = "Tmin", type_ ="description")
#' get_parameter_info(model_ = "Lemna_Schmitt", parameter_ = "Tmin", type_ ="unit")
#' get_parameter_info(model_ = "Lemna_Schmitt", parameter_ = "Tmin", type_ ="group")
#' get_parameter_info(model_ = "Lemna_Schmitt", parameter_ = "Tmin", type_ ="expert.value")
#' get_parameter_info(model_ = "Lemna_Schmitt", parameter_ = "Tmin", type_ ="default")
#' }
get_parameter_info <- function(model_, parameter_, type_){
  stopifnot(model_ %in% (cvasi.ui::parameter_descriptions %>% dplyr::pull(model) %>% unique()))
  if (!(type_ %in% colnames(cvasi.ui::parameter_descriptions))){
    warning(paste0(type_," is not available in 'parameter_descriptions'."))
    return(NULL)
  }
  
  mod_par <- cvasi.ui::parameter_descriptions %>% 
    dplyr::filter(model == model_, parameter == parameter_)
  
  if (!nrow(mod_par)){
    warning(paste0("Parameter ",parameter_," is not available for model ",model_," in 'parameter_descriptions'."))
    return(NULL)
  }
  
  mod_par %>% 
    dplyr::pull(type_)
}
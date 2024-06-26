#' Selectable models
#' @format A list with model names
"model_choices"

#' Lookup table for model names, scenario/class names and constructor function names
#' @format A data.frame with 
#' \describe{
#'   \item{model_name}{the model name that is returned from `get_name`}
#'   \item{scenario}{the scenario/class name that is returned from `class`}
#'   \item{model_f}{the name of the constructor function; listed in `model_choices`}
#' }
"model_lookup"

#' Default values 
#' @format A list for each selectable model
#' \describe{
#'  \item{init_defaults}{Default values for model initial values}
#'  \item{parameter_defaults}{Default values for the model parameters}
#'  \item{forcing_defaults}{Default values for the model forcings}
#' }
"model_defaults"

#' Default exposure timeseries
#' @format A data.frame with
#' \describe{
#'  \item{time}{time}
#'  \item{conc}{concentration}
#'  \item{trial}{indicates the trial of several trials with increasing application rates are used}
#' }
"default_exposure"

#' Detailed information on the parameters and state variables of the available models
#' @format a data.frame with
#' \describe{
#'  \item{model}{The 'name' of a model object}
#'  \item{parameter}{The parameter/state variable name}
#'  \item{description}{A description of the parameter/state variable}
#'  \item{unit}{The unit of the parameter/state variable}
#'  \item{group}{Grouping factor for the parameters 
#'              (with levels 'physiological', 'toxicodynamic' and 'toxicokinetic') and
#'              the state variable ('state variable)}
#'  \item{expert.value}{Is this a parameter/state variable that is suggested to be 
#'              modified only by experts? 'yes' or 'no'}
#'  \item{default}{A default value}
#' }
"parameter_descriptions"


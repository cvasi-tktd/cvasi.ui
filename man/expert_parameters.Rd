% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_parameter_gouping.R
\name{expert_parameters}
\alias{expert_parameters}
\title{Separate parameters into expert or non-expert groups}
\usage{
expert_parameters(p, model_)
}
\arguments{
\item{p}{a character vector listing the parameter names}

\item{model_}{the model for which the parameters should be grouped into expert and non-expert}
}
\value{
a list with parameters separated into expert and non-expert groups as specified by [parameter_descriptions()]
}
\description{
Separate parameters into expert or non-expert groups
}
\examples{
\dontrun{
expert_parameters(p = c("k_phot_fix", "k_phot_max", "k_resp", "k_loss", "Tmin", "Tmax", 
                       "Topt", "t_ref", "Q10", "k_0", "a_k", "C_P", "CP50", "a_P", "KiP", 
                       "C_N", "CN50", "a_N", "KiN", "BM50", "mass_per_frond", "BMw2BMd", 
                       "Emax", "EC50", "b", "P_up", "AperBM", "Kbm", "P_Temp", "MolWeight"
                       ),
                 model_ = "Lemna_Schmitt")
}
}
\seealso{
[parameter_descriptions]
}

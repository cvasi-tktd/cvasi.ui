% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_model_helpers.R
\name{get_val}
\alias{get_val}
\title{Get parameters, forcings or initial values of a}
\usage{
get_val(x, type = c("param", "init", "forcings"))
}
\arguments{
\item{x}{the effect model object}

\item{type}{the type to check; either "param", "init" or forcings}
}
\value{
list of parameters, initial values or forcings
}
\description{
Get parameters, forcings or initial values of a
}
\examples{
\dontrun{
dat <- cvasi::Lemna_Schmitt()
get_val(dat, type = "param")
get_val(dat, type = "init")
get_val(dat, type = "forcings")
}
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/full_dlm_specification.R
\name{full_dlm_modeling}
\alias{full_dlm_modeling}
\title{Dynamic Linear Model Estimation}
\usage{
full_dlm_modeling(
  data,
  t_var,
  y_var,
  state_components = NULL,
  deterministic_components = NULL,
  seasonal_frequency = NULL,
  reg_vars = NULL
)
}
\arguments{
\item{data}{Data frame/tibble containing the series to model and the time variable.}

\item{t_var}{Date vector of length 1. Column name of the time/date variable.}

\item{y_var}{Character (atomic) vector of length 1. Column name of the y variable.}

\item{state_components}{Character vector of the state components. Must be any combination of the following elements: \code{c("level", "slope", "seasonal", "regressor")}. Must contain \code{"level"} at least.}

\item{deterministic_components}{Character vector specifying the deterministic state components. Must be any combination of the following elements: \code{c("level", "slope", "seasonal")}. Deterministic explanatory variable coefficients must be specified as "reg1", "reg2", ... Each "reg" corresponds to a variable in \code{reg_data}.}

\item{seasonal_frequency}{Numeric vector of length 1. Periodicity of the seasonal component. Must be different from \code{NULL} if \code{state_components} contains \code{"seasonal"}.}

\item{reg_vars}{Character vector of the column name(s) of the explanatory variable(s) in \code{data}. This requires \code{state_components} to include \code{"regressor"}.}
}
\value{
A list containing the model's output and metadata.
}
\description{
Dynamic Linear Model Estimation
}
\examples{
print("Soon!")
}

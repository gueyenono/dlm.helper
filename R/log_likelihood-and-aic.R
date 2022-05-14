#' Calculate the value of the log-likelihood function
#'
#' @param dlm_filtered The output from \code{dlm::dlmFilter()}.
#'
#' @return
#'
#' @examples
#' Soon!
get_log_likelihood <- function(dlm_filtered){
  n_state_vars <- nrow(dlm_filtered$mod$W)
  irregular <- residuals(object = dlm_filtered, type = "raw", sd = TRUE)

  n <- length(irregular$res)
  pred_error <- irregular$res[-seq_len(n_state_vars)]
  pred_error_var <- irregular$sd[-seq_len(n_state_vars)]^2

  - 0.5 * n * log(2*pi) - 0.5 * sum(log(pred_error_var) + (pred_error^2/pred_error_var))
}


#' Calculate the value of the AIC
#'
#' @param log_likelihood Numeric vector of length 1. Value of the log-likelihood function as provided by the \code{get_log_likelihood()} function.
#' @param n_state_vars Numeric value of length 1. Number of state variables in the dynamic linear model.
#' @param n_hyper_params Numeric value of length 1. Number of stochastic hyperparameters in the dynamic linear model.
#'
#' @return The Akaike Information Criterion (AIC) of the model.
#'
#' @examples
#' Soon!
get_AIC <- function(log_likelihood, n_state_vars, n_hyper_params){
  -2 * log_likelihood + 2 * (n_state_vars + n_hyper_params)
}

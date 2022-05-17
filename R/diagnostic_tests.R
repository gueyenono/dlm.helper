#' Ljung-Box test of independence of the model residuals
#' @description The Ljung-Box procedure implements a test against the null hypothesis of independence.
#' @param dlm_filtered The output from \code{dlm::dlmFilter()}.
#' @param n_lags The number of lags to consider in the test.
#'
#' @return A data.frame/tibble of the model results including a column specifying whether the assumption of independence of the residuals is satisfied.
#' @export
#'
#' @examples
#' Soon
dt_independence <- function(dlm_filtered, n_lags){

  n_state_vars <- nrow(dlm_filtered$mod$W)
  n_hyper_params <- 1 + sum(diag(dlm_filtered$mod$W) != 0)
  standardized_residuals <- residuals(object = dlm_filtered, type = "standardized", sd = FALSE)

  test_results <- Box.test(
    x = standardized_residuals[-seq_len(n_state_vars)],
    lag = n_lags,
    type = "Ljung-Box"
  )

  tibble::tibble(
    Goal = "Independence",
    Test = "Ljung-Box",
    Statistic = test_results$statistic,
    `Critical value` = qchisq(p = 0.95, df = n_lags-n_hyper_params+1),
    `P-value` = test_results$p.value,
    `Assumption satisfied` = ifelse(Statistic >= `Critical value`, "-", "+")
  )
}


#' H-test of homoskedasticity of the model residuals
#' @description The H-test procedure implements a test against the null hypothesis of homoskedastic residuals.
#' @param dlm_filtered The output from \code{dlm::dlmFilter()}.
#'
#' @return A data.frame/tibble of the model results including a column specifying whether the assumption of homoskedasticity of the residuals is satisfied.
#' @export
#'
#' @examples
#' Soon!
dt_homoskedasticity <- function(dlm_filtered){

  n_state_vars <- nrow(dlm_filtered$mod$W)
  standardized_residuals <- residuals(object = dlm_filtered, type = "standardized", sd = FALSE)
  n <- length(standardized_residuals)

  block_size <- round((n - n_state_vars)/3)

  head_values <- head(standardized_residuals[-seq_len(n_state_vars)], block_size)
  tail_values <- tail(standardized_residuals, block_size)

  lower_crit <- qf(0.025, block_size, block_size)
  upper_crit <- qf(0.975, block_size, block_size)

  tibble::tibble(
    Goal = "Homeskedasticity",
    Test = "H-test",
    Statistic = sum(tail_values^2) / sum(head_values^2),
    `Critical value` = upper_crit,
    `P-value` = pf(q = Statistic, block_size, block_size, lower.tail = FALSE),
    `Assumption satisfied` = ifelse(Statistic >= `Critical value`, "-", "+")
  )

}


#' Jarque-Bera test of normality of the model residuals
#'
#' @param dlm_filtered The output from \code{dlm::dlmFilter()}.
#'
#' @return A data.frame/tibble of the model results including a column specifying whether the assumption of normality of the residuals is satisfied.
#' @export
#'
#' @examples
#' Soon!
dt_normality <- function(dlm_filtered){

  n_state_vars <- nrow(dlm_filtered$mod$W)
  standardized_residuals <- residuals(object = dlm_filtered, type = "standardized", sd = FALSE)
  test_results <- tseries::jarque.bera.test(x = standardized_residuals[-seq_len(n_state_vars)])

  tibble::tibble(
    Goal = "Normality",
    Test = "Jarque-Bera",
    Statistic = test_results$statistic,
    `Critical value` = qchisq(p = 0.95, df = 2),
    `P-value` = test_results$p.value,
    `Assumption satisfied` = ifelse(Statistic >= `Critical value`, "-", "+")
  )

}


#' Diagnostic tests of a dynamic linear model (independence, homoskedasticity and normality)
#' @description This function implements the Ljung-Box test of independence, the H-test of homoskedasticity and the Jarque-Bera test of normality on the model residuals.
#' @param dlm_filtered The output from \code{dlm::dlmFilter()}.
#' @param n_lags
#'
#' @return A data.frame/tibble of the model results including a column specifying whether the assumptions of independence, homoskedasticity and normality of the residuals is satisfied.
#' @export
#'
#' @examples
#' Soon!
diagnostic_tests <- function(dlm_filtered, n_lags){
  ind <- dt_independence(dlm_filtered = dlm_filtered, n_lags = n_lags)
  hom <- dt_homoskedasticity(dlm_filtered = dlm_filtered)
  norm <- dt_normality(dlm_filtered = dlm_filtered)
  Reduce(rbind, list(ind, hom, norm))
}

#' Ljung-Box test of independence of the model residuals
#' @description The Ljung-Box procedure implements a test against the null hypothesis of independence.
#' @param dlm_mod The output of the \code{dlm::full_dlm_modeling} function.
#' @param n_lags The number of lags to consider in the test.
#'
#' @return A data.frame/tibble of the model results including a column specifying whether the assumption of independence of the residuals is satisfied.
#' @export
#'
#' @examples
#' print("Soon!")
dt_independence <- function(dlm_filtered, n_lags){

  n_state_vars <- base::nrow(dlm_filtered$mod$W)
  n_hyper_params <- 1 + base::sum(diag(dlm_filtered$mod$W) != 0)
  standardized_residuals <- dlm:::residuals.dlmFiltered(object = dlm_filtered, type = "standardized", sd = FALSE)

  test_results <- stats::Box.test(
    x = standardized_residuals[-base::seq_len(n_state_vars)],
    lag = n_lags,
    type = "Ljung-Box"
  )

  dplyr::tibble(
    Goal = "Independence",
    Test = "Ljung-Box",
    Statistic = test_results$statistic,
    `Critical value` = stats::qchisq(p = 0.95, df = n_lags-n_hyper_params+1),
    `P-value` = test_results$p.value,
    `Assumption satisfied` = base::ifelse(Statistic >= `Critical value`, "-", "+")
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
#' print("Soon!")
dt_homoskedasticity <- function(dlm_filtered){

  n_state_vars <- base::nrow(dlm_filtered$mod$W)
  standardized_residuals <- dlm:::residuals.dlmFiltered(object = dlm_filtered, type = "standardized", sd = FALSE)
  n <- base::length(standardized_residuals)

  block_size <- base::round((n - n_state_vars)/3)

  head_values <- utils::head(standardized_residuals[-seq_len(n_state_vars)], block_size)
  tail_values <- utils::tail(standardized_residuals, block_size)

  lower_crit <- stats::qf(0.025, block_size, block_size)
  upper_crit <- stats::qf(0.975, block_size, block_size)

  dplyr::tibble(
    Goal = "Homeskedasticity",
    Test = "H-test",
    Statistic = base::sum(tail_values^2) / base::sum(head_values^2),
    `Critical value` = upper_crit,
    `P-value` = stats::pf(q = Statistic, block_size, block_size, lower.tail = FALSE),
    `Assumption satisfied` = base::ifelse(Statistic >= `Critical value`, "-", "+")
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
#' print("Soon!")
dt_normality <- function(dlm_filtered){

  n_state_vars <- base::nrow(dlm_filtered$mod$W)
  standardized_residuals <- dlm:::residuals.dlmFiltered(object = dlm_filtered, type = "standardized", sd = FALSE)
  test_results <- tseries::jarque.bera.test(x = standardized_residuals[-base::seq_len(n_state_vars)])

  dplyr::tibble(
    Goal = "Normality",
    Test = "Jarque-Bera",
    Statistic = test_results$statistic,
    `Critical value` = stats::qchisq(p = 0.95, df = 2),
    `P-value` = test_results$p.value,
    `Assumption satisfied` = base::ifelse(Statistic >= `Critical value`, "-", "+")
  )

}


#' Diagnostic tests of a dynamic linear model (independence, homoskedasticity and normality)
#' @description This function implements the Ljung-Box test of independence, the H-test of homoskedasticity and the Jarque-Bera test of normality on the model residuals.
#' @param dlm_filtered The output from \code{dlm::dlmFilter()}.
#' @param n_lags Number of lags of the independence test (i.e. the Ljung-Box test)
#'
#' @return A data.frame/tibble of the model results including a column specifying whether the assumptions of independence, homoskedasticity and normality of the residuals is satisfied.
#' @export
#'
#' @examples
#' print("Soon!")
diagnostic_tests <- function(dlm_mod, n_lags){
  dlm_filtered <- dlm_mod$filtered
  ind <- dt_independence(dlm_filtered = dlm_filtered, n_lags = n_lags)
  hom <- dt_homoskedasticity(dlm_filtered = dlm_filtered)
  norm <- dt_normality(dlm_filtered = dlm_filtered)
  base::Reduce(base::rbind, base::list(ind, hom, norm))
}

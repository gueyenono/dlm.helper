
diagnostic_test_independence <- \(dlm_filtered, n_lags = 15){

  # Ljung-Box test

  std_forecast_error <- residuals(object = dlm_filtered, type = "standardized", sd = FALSE)

  # Get number of state variables
  if(is.atomic(dlm_filtered$m)) n_state_vars <- 1
  if(!is.atomic(dlm_filtered$m)) n_state_vars <- ncol(dlm_filtered$m)

  # Get number of hyperparameters
  n_hyp_parms <- 1

  test_res <- Box.test(
    x = std_forecast_error[-(1:n_state_vars)],
    lag = n_lags,
    type = "Ljung-Box"
  )

  out_tbl <- tibble::tibble(
    statistic = test_res$statistic,
    crit_val_99 = qchisq(p = 0.99, df = n_lags - n_hyp_parms + 1),
    crit_val_95 = qchisq(p = 0.95, df = n_lags - n_hyp_parms + 1),
    crit_val_90 = qchisq(p = 0.9, df = n_lags - n_hyp_parms + 1),
    p_value = test_res$p.value
  )

  out_p <- forecast::ggAcf(x = std_forecast_error, lag.max = n_lags, type = "correlation") +
    ggplot2::labs(x = "Lag", title = NULL) +
    ggplot2::theme_bw()

  list(stat_test = out_tbl, acf = out_p)

}

# diagnostic_test_independence(dlm_filtered)

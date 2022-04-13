diagnostic_test_normality <- \(dlm_filtered){

  # The Jarque-Bera test
  # H0: Series is normally distributed

  std_forecast_error <- residuals(object = dlm_filtered, type = "standardized", sd = FALSE)

  test_res <- tseries::jarque.bera.test(x = std_forecast_error)

  out_tbl <- tibble::tibble(
    statistic = test_res$statistic,
    crit_val_99 = qchisq(p = 0.99, df = test_res$parameter),
    crit_val_95 = qchisq(p = 0.95, df = test_res$parameter),
    crit_val_90 = qchisq(p = 0.9, df = test_res$parameter),
    p_value = test_res$p.value
  )

  std_resid_df <- tibble::enframe(x = std_forecast_error, name = NULL, value = "std_resid")

  out_p <- ggplot2::ggplot(data = std_resid_df, mapping = ggplot2::aes(sample = std_resid)) +
    ggplot2::stat_qq(alpha = 0.2) +
    ggplot2::stat_qq_line(color = "red") +
    ggplot2::labs(x = "Theoretical quantiles", y = "Sample quantiles") +
    ggplot2::theme_bw()

  list(stat_test = out_tbl, qq = out_p)

}

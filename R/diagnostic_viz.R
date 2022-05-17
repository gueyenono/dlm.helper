full_dlm_viz <- function(dlm_mod){

  stopifnot( "'dlm_mod' must be the output of the 'full_dlm_modeling' function." = class(dlm_mod) == "dlm_mod")

  diag_viz <- list()

  # Residuals
  diag_viz$resid <- ggplot2::ggplot(data = dlm_mod$smoothed_estimates) +
    ggplot2::geom_line(mapping = ggplot2::aes(x = time, y = residuals)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::labs(
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_bw()

  diag_viz$ljungbox <- purrr::map_dbl(1:10, ~  Box.test(x = dlm_mod$smoothed_estimates$residuals_stdzd, lag = .x, type = "Ljung-Box")$p.value) %>%
    tibble::enframe(name = "lag", value = "p.value") %>%
    ggplot2::ggplot(ggplot2::aes(x = lag, y = p.value)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0.05, color = "red", linetype = 2) +
    ggplot2::scale_x_continuous(breaks = 1:10, labels = 1:10) +
    ggplot2::labs(
      x = "Lag",
      y = "P-value"
    ) +
    ggplot2::theme_minimal()

  diag_viz$acf <- forecast::ggAcf(x = dlm_mod$smoothed_estimates$residuals_stdzd, lag.max = 20, type = "correlation") +
    ggplot2::labs(
      x = "Lag",
      title = NULL
    ) +
    ggplot2::theme_minimal()

  diag_viz$qqplot <- ggplot2::ggplot(data = dlm_mod$smoothed_estimates, mapping = ggplot2::aes(sample = residuals_stdzd)) +
    ggplot2::stat_qq(alpha = .2) +
    ggplot2::stat_qq_line(color = "red", linetype = 2) +
    ggplot2::labs(x = "Theoretical quantiles", y = "Sample quantiles") +
    ggplot2::theme_minimal()

  diag_viz

}

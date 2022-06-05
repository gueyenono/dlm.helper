
#' Visualizations of DLM model estimates
#' @description The function visualizes the model's state components.
#' @param dlm_mod The output of the \code{dlm::full_dlm_modeling} function.
#'
#' @return A \code{list} of \code{ggplot} objects.
#' @export
#'
#' @examples
#' print("Soon!")
dlm_state_viz <- function(dlm_mod){

  stopifnot( "'dlm_mod' must be the output of the 'full_dlm_modeling' function." = class(dlm_mod) == "dlm_model")

  diag_viz <- list()

  cols_to_keep <- c("time", "level", "slope", "seas1")
  reg_cols <- grep(pattern = "reg\\d+", x = colnames(dlm_mod$smoothed_estimates), value = TRUE)
  cols <- c(
    intersect(x = colnames(dlm_mod$smoothed_estimates), cols_to_keep),
    reg_cols
  )

  out <- purrr::map(setdiff(x = cols, y = "time"), function(col){
    col_quo <- ggplot2::sym(col)
    p <- ggplot2::ggplot(data = dlm_mod$smoothed_estimates) +
      ggplot2::geom_line(mapping = ggplot2::aes(x = time, y = !!col_quo)) +
      ggplot2::labs(x = NULL) +
      ggplot2::theme_minimal()
    p
  })

  names(out) <- base::setdiff(x = cols, y = "time")

  out

}


#' Visualizations for model diagnostics
#' @description The function makes some visualizations for determining the model's goodness of it: residuals, autocorrelation function (ACF), QQ-plot and the p-values of the Ljung-Box statistic.
#' @param dlm_mod The output of the \code{dlm::full_dlm_modeling} function.
#'
#' @return A \code{list} of \code{ggplot} objects.
#' @export
#'
#' @examples
#' print("Soon!")
dlm_diag_viz <- function(dlm_mod){

  stopifnot( "'dlm_mod' must be the output of the 'full_dlm_modeling' function." = class(dlm_mod) == "dlm_model")

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

  diag_viz$ljungbox <- purrr::map_dbl(1:10, ~  stats::Box.test(x = dlm_mod$smoothed_estimates$residuals_stdzd, lag = .x, type = "Ljung-Box")$p.value) |>
    tibble::enframe(name = "lag", value = "p.value") |>
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
    ggplot2::stat_qq(alpha = 1) +
    ggplot2::stat_qq_line(color = "black", linetype = 2) +
    qqplotr::stat_qq_band(alpha = 0.5) +
    ggplot2::labs(x = "Theoretical quantiles", y = "Sample quantiles") +
    ggplot2::theme_minimal()

  diag_viz

}

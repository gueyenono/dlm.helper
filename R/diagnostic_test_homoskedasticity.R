diagnostic_test_homoskedasticity <- \(dlm_filtered){

  # H-test

  std_forecast_error <- residuals(object = dlm_filtered, type = "standardized", sd = FALSE)

  # Get number of state variables
  if(is.atomic(dlm_filtered$m)) n_state_vars <- 1
  if(!is.atomic(dlm_filtered$m)) n_state_vars <- ncol(dlm_filtered$m)

  n <- length(std_forecast_error)

  # Define the block size (number of elements considered in each part)
  block_size <- round((n - n_state_vars)/3)

  # Compute the H-statistic
  stat <- sum(std_forecast_error[(n-block_size + 1):n]^2) /
    sum(std_forecast_error[(n_state_vars+1):(n_state_vars+block_size)]^2)

  # Compute upper and lower critical values
  vals <- (1 - c(0.99, 0.95, 0.9)) / 2
  upper <- unlist(lapply(1 - vals, function(x) qf(x, block_size, block_size)))
  lower <- unlist(lapply(vals, function(x) qf(x, block_size, block_size)))
  tibble::tibble(
    sig_level = c("99%", "95%", "90%"),
    upper_crit_val = upper,
    lower_crit_val = lower,
    stat = stat
  )
}

# dlm_filtered <- llm1_filtered
# dlm_smooth <- llm1_smoothed

extract_dlm_data <- \(dlm_filtered, dlm_smooth){

  pred_error <- residuals(object = dlm_filtered, type = "raw", sd = TRUE)
  pred_error_std <- residuals(object = dlm_filtered, type = "standardized", sd = TRUE)

  # tibble::tibble(
  #   e1 = pred_error$res,
  #   e2 = dlm_filtered$y - dlm_filtered$m[-1],
  #   e3 = dlm_filtered$y - dlm_smooth$s[-1]
  # )

  tibble::tibble(
    y = dlm_filtered$y,
    filt = dlm_filtered$m[-1],
    e = pred_error$res,
    e_sd = pred_error$sd,
    stdzed_e = pred_error_std$res,
    stdzed_e_sd = pred_error_std$sd,
    smooth = dlm_smooth$s[-1],
    filt_var = unlist(dlm::dlmSvd2var(u = dlm_filtered$U.C, d = dlm_filtered$D.C))[-1],
    smooth_var = unlist(dlm::dlmSvd2var(u = dlm_smooth$U.S, d = dlm_smooth$D.S))[-1]
  )

}

# dlm_data <- extract_dlm_data(dlm_filtered, dlm_smooth)
#
# plot(dlm_data$filt_var, type = "l")
# plot(dlm_data$smooth_var, type = "l")

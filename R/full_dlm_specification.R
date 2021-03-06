#' Dynamic Linear Model Estimation
#'
#' @param data Data frame/tibble containing the series to model and the time variable.
#' @param y_var Character (atomic) vector of length 1. Column name of the y variable.
#' @param t_var Date vector of length 1. Column name of the time/date variable.
#' @param state_components Character vector of the state components. Must be any combination of the following elements: \code{c("level", "slope", "seasonal", "regressor")}. Must contain \code{"level"} at least.
#' @param deterministic_components Character vector specifying the deterministic state components. Must be any combination of the following elements: \code{c("level", "slope", "seasonal")}. Deterministic explanatory variable coefficients must be specified as "reg1", "reg2", ... Each "reg" corresponds to a variable in \code{reg_data}.
#' @param seasonal_frequency Numeric vector of length 1. Periodicity of the seasonal component. Must be different from \code{NULL} if \code{state_components} contains \code{"seasonal"}.
#' @param reg_vars Character vector of the column name(s) of the explanatory variable(s) in \code{data}. This requires \code{state_components} to include \code{"regressor"}.
#'
#' @return A list containing the model's output and metadata.
#'
#' @export
#'
#' @examples
#' print("Soon!")
full_dlm_modeling <- function(
    data,
    t_var,
    y_var,
    state_components = NULL,
    deterministic_components = NULL,
    seasonal_frequency = NULL,
    reg_vars = NULL
){

  # ERROR HANDLING HERE!!!

  # stopifnot("The 'state_components' argument suggests that the model contains at least one explanatory variable, but no data was provided in the 'reg_data' argument." = "regressor" %in% state_components & is.null(reg_data))
  #
  # stopifnot("The 'reg_data' argument must be of one of the following classes: 'numeric', 'matrix' or 'data.frame'." = ("regressor" %in% state_components & class(reg_data)[1] %in% c("matrix", "numeric", "double", "data.frame")) | (!"regressor" %in% state_components & is.null(reg_data)))
  #
  # stopifnot("The state_components argument must contain any combination of 'level' (mandatory), 'slope', 'seasonal' and 'regressor'." =  all(state_components %in% c("level", "slope", "seasonal", "regressor")))

  # Error: If a deterministic component is not specified as a state component
  # Error: "regressor" instead of "reg1", "reg2", ... in deterministic_components
  # Error: time_var has different length from series
  # Error: time_var is not of the Date class
  # Error: ditto for reg_data

  # Order of the polynomial model

  order <- dplyr::case_when(
    !"slope" %in% state_components ~ 1,
    "slope" %in% state_components ~ 2
  )

  # Transform regression data from atomic vector to matrix

  if("regressor" %in% state_components) reg_data <- data[, reg_vars, drop = FALSE]

  # Calculate number of state variables

  n_seasonality <- ifelse("seasonal" %in% state_components, seasonal_frequency - 1, 0)
  n_regressors <- ifelse("regressor" %in% state_components, ncol(reg_data), 0)
  level_and_or_slope_comps <- grep(pattern = "level|slope", x = state_components, value = TRUE)
  n_state_vars <- length(level_and_or_slope_comps) + n_seasonality + n_regressors

  # Stochastic hyperparameters

  state_components2 <- c(
    setdiff(state_components, "regressor"),
    {
      if("regressor" %in% state_components){
        out <- paste0("reg", seq_len(ncol(reg_data)))
      } else {
        out <- NULL
      }
      out
    }
  )

  stoch_state_comps <- setdiff(state_components2, deterministic_components)
  n_hyper_params <- 1 + length(stoch_state_comps)

  # Initial values of the stochastic hyperparameters

  parm <- hyper_params_initial <- c(
    var(data[[y_var]]),
    rep(0.001, n_hyper_params-1)
  )

  # Function for building the model

  func_dlm_mod <- function(parm){

    mod_list <- list()

    # Level/slope component
    mod_list$poly <- dlm::dlmModPoly(
      order = order,
      dV = exp(parm[1]),
      dW = {
        dW_poly <- glue::glue("exp(parm[{1+seq_len(order)}])")
        index_deter_poly <- purrr::imap_dbl(level_and_or_slope_comps, function(comp, i){
          if(comp %in% deterministic_components){
            return(i)
          } else {
            return(NA)
          }
        })
        dW_poly[index_deter_poly] <- "0"
        dW_poly |>
          str2expression() |>
          purrr::map_dbl(~ eval(.x))
      }
    )

    # Seasonal component
    if("seasonal" %in% state_components){

      mod_list$seas <- dlm::dlmModSeas(
        frequency = seasonal_frequency,
        dV = 0,
        dW = {
          dW_seas <- rep(0, times = seasonal_frequency - 1)
          seas_comp_i <- which(stoch_state_comps == "seasonal")
          if(!"seasonal" %in% deterministic_components) dW_seas[1] <- eval(str2expression(glue::glue("exp(parm[{1 + seas_comp_i}])")))
          dW_seas
        }
      )

    }

    # Explanatory variable(s) components
    if("regressor" %in% state_components){

      mod_list$reg <- dlm::dlmModReg(
        X = reg_data,
        dV = 0,
        dW = {
          dW_reg <- rep("0", n_regressors)
          reg_comps <- paste0("reg", seq_len(n_regressors))
          n_level_slope_stoch_comps <- sum(grepl(pattern = "level|slope", x = stoch_state_comps))
          n_stoch_seas <- ifelse("seasonal" %in% deterministic_components, 1, 0)
          reg_stoch_comps <- grep(pattern = "^reg\\d+$", x = stoch_state_comps, value = TRUE)

          if(length(reg_stoch_comps) != 0){
            reg_stoch_comp_i <- which(reg_comps %in% stoch_state_comps)
            reg_stoch_comp_i2 <- which(reg_stoch_comps %in% stoch_state_comps) + n_level_slope_stoch_comps + n_stoch_seas + 1
            dW_reg[reg_stoch_comp_i] <- glue::glue("exp(parm[{reg_stoch_comp_i2}])")
          }

          dW_reg |>
            str2expression() |>
            purrr::map_dbl(~ eval(.x))
        },
        addInt = FALSE
      )
    }

    Reduce("+", mod_list)

  }

  # Estimation of the model's hyperparameters with Maximul Likelihood Estimation (MLE)

  hyper_parms_mle_est <- dlm::dlmMLE(
    y = data[[y_var]],
    parm = log(hyper_params_initial),
    build = func_dlm_mod
  )

  # Do we have convergence?
  stopifnot("No convergence was reached on the maximum likelihood estimation of the model's hyperparameters!" = hyper_parms_mle_est$convergence == 0)

  # Build the model using the MLE hyperparameter estimates
  dlm_mod <- func_dlm_mod(parm = hyper_parms_mle_est$par)

  # Filter and smooth the state with the Kalman procedures
  dlm_filtered <- dlm::dlmFilter(y = data[[y_var]], mod = dlm_mod)
  dlm_smoothed <- dlm::dlmSmooth(y = data[[y_var]], mod = dlm_mod)

  # Get log-likelihood at convergence (needed to compute the AIC)
  ll <- get_log_likelihood(dlm_filtered = dlm_filtered)

  # Get log-likelihood at convergence (similar to the book's)
  ll2 <- ll / nrow(data)

  # Compute model's AIC
  dlm_aic <- get_AIC(log_likelihood = ll, n_state_vars = n_state_vars, n_hyper_params = n_hyper_params)

  # Compute model's AIC (similar to the book's)
  dlm_aic2 <- dlm_aic / nrow(data)


  # Data frame of all smooth estimates

  col_names0 <- lapply(state_components, function(s){
    out <- NULL
    if(s == "level"){
      out <- "level"
    }
    if(s == "slope"){
      out <- "slope"
    }
    if(s == "seasonal"){
      out <- paste0("seas", seq_len(seasonal_frequency-1))
    }
    if(s == "regressor"){
      l <- ifelse(
        is.atomic(reg_data),
        1,
        ncol(reg_data)
      )
      out <- paste0("reg", seq_len(l))
    }
    out
  })

  col_names <- Reduce(f = c, x = col_names0)

  if(base::all(state_components == "level")){
    state_estimates <- tibble::tibble(level = dlm_smoothed$s[-1])
  } else {
    state_estimates <- tibble::as_tibble(dlm_smoothed$s[-1, ], .name_repair = "unique")
  }

  smoothed_estimates <- data[, c(t_var, y_var)] |>
    dplyr::bind_cols(state_estimates) |>
    setNames(Reduce(f = c, x = c("time", "series", col_names)))

  all_states <- purrr::map_dfc(state_components, function(comp){
    if(comp == "level"){
      out <- smoothed_estimates[["level"]]
    }
    if(comp == "slope"){
      out <- smoothed_estimates[["slope"]]
    }
    if(comp == "regressor"){
      reg_colnames <- grep(pattern = "reg\\d+", x = colnames(smoothed_estimates), value = TRUE)
      out <- smoothed_estimates[, reg_colnames] * reg_data
    }
    if(comp == "seasonal"){
      out <- smoothed_estimates[["seas1"]]
    }
    out
  }) |>
    rowSums()

  smoothed_estimates$all_states <- all_states

  smoothed_estimates$residuals <- data[[y_var]] - smoothed_estimates$all_states
  smoothed_estimates$residuals_raw <- dlm:::residuals.dlmFiltered(object = dlm_filtered, type = "raw", sd = FALSE)
  smoothed_estimates$residuals_stdzd <- dlm:::residuals.dlmFiltered(object = dlm_filtered, type = "standardized", sd = FALSE)

  smoothed_estimates$smoothed_variance <- {
    variance <- dlm::dlmSvd2var(u = dlm_smoothed$U.S, d = dlm_smoothed$D.S)
    purrr::map_dbl(variance[-1], ~ .x[1, 1])
  }

  smoothed_estimates$conf_band_lower <- smoothed_estimates$all_states - qnorm(0.025, lower.tail = FALSE) * sqrt(smoothed_estimates$smoothed_variance)
  smoothed_estimates$conf_band_upper <- smoothed_estimates$all_states + qnorm(0.025, lower.tail = FALSE) * sqrt(smoothed_estimates$smoothed_variance)

  out <- list(
    data = data[, c(t_var, y_var, reg_vars)],
    series = data[[y_var]],
    n_state_vars = n_state_vars,
    n_hyper_params = n_hyper_params,
    seasonal_frequency = seasonal_frequency,
    obs_variance = as.numeric(dlm_mod$V),
    state_variance = diag(dlm_mod$W),
    filtered = dlm_filtered,
    smoothed = dlm_smoothed,
    loglik = ll,
    loglik2 = ll2,
    aic = dlm_aic,
    aic2 = dlm_aic2,
    state_components = state_components,
    deterministic_components = deterministic_components,
    smoothed_estimates = smoothed_estimates
  )

  class(out) <- "dlm_model"

  out

}

source(here::here("R/visualizations.R"))
source(here::here("R/diagnostic_tests.R"))
source(here::here("R/log-lik_aic.R"))


#' Dynamic Linear Model Estimation
#'
#' @param series Numeric vector of the data
#' @param state_components Character vector of the state components. Must be any combination of the following elements: \code{c("level", "slope", "seasonal", "regressor")}. Must contain \code{"level"} at least.
#' @param deterministic_components Character vector specifying the deterministic state components. Must be any combination of the following elements: \code{c("level", "slope", "seasonal")}. Deterministic explanatory variable coefficients must be specified as "reg1", "reg2", ... Each "reg" corresponds to a variable in \code{reg_data}.
#' @param seasonal_frequency Numeric vector of length 1. Periodicity of the seasonal component. Must be different from \code{NULL} if \code{state_components} contains \code{"seasonal"}.
#' @param reg_data Numeric vector, data frame or matrix. Contains the explanatory variable(s). Must be different from \code{NULL} if \code{state_components} contains \code{"regressor"}.
#'
#' @return A list containing the model's output and metadata.
#' @export
#'
#' @examples
full_dlm_modeling <- function(
    series,
    state_components = "level",
    deterministic_components = NULL,
    seasonal_frequency = NULL,
    reg_data = NULL
){

  # Initial error handling stuff

  stopifnot("No frequency was defined for the seasonal component of the model." = ("seasonal" %in% state_components & is.numeric(seasonal_frequency)) | (!"seasonal" %in% state_components & is.null(seasonal_frequency)))

  stopifnot("The regression data needs to be provided to the 'reg_data' argument and it must be a numeric vector, a matrix or a data frame." = ("regressor" %in% state_components & class(reg_data)[1] %in% c("matrix", "numeric", "double", "data.frame")) | (!"regressor" %in% state_components & is.null(reg_data)))

  stopifnot("The state_components argument must contain any combination of 'level' (mandatory), 'slope', 'seasonal' and 'regressor'." =  all(state_components %in% c("level", "slope", "seasonal", "regressor")))

  # browser()

  # Initialize key variables for the model

  order <- dplyr::case_when(
    !"slope" %in% state_components ~ 1,
    "slope" %in% state_components ~ 2
  )

  if(is.atomic(reg_data)) reg_data <- as.matrix(reg_data)

  # Number of state variables

  n_seasonality <- ifelse("seasonal" %in% state_components, seasonal_frequency - 1, 0)
  n_regressors <- ifelse("regressor" %in% state_components, ncol(reg_data), 0)
  non_seas_and_reg_state_comps <- state_components[!state_components %in% c("seasonal", "regressor")]
  n_state_vars <- length(non_seas_and_reg_state_comps) + n_seasonality + n_regressors

  # Names and number of hyperparameters

  non_reg_state_comps <- state_components[!state_components %in% "regressor"]
  n_deter_reg <- sum(grepl(pattern = "reg\\d+", x = deterministic_components, ignore.case = TRUE)) # Number of deterministic regressors

  state_components2 <- c(
    setdiff(state_components, "regressor"),
    paste0("reg", seq_len(ncol(reg_data)))
  )

  hyper_params <- setdiff(state_components2, deterministic_components)
  n_hyper_params <- 1 + length(hyper_params)

  # n_hyper_params <- length(non_reg_state_comps) + n_regressors - length(deterministic_components)

  # Inital values for the hyperparameters
  hyper_params_initial <- c(
    var(series),
    rep(0.001, n_hyper_params-1)
  )

  # Column names of the data frame of estimated values (at the very bottom!)

  col_names <- lapply(state_components, function(s){
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
  }) %>%
    Reduce(f = c, x = .)


  # Function for building the model

  func_dlm_mod <- function(parm){

    mod_list <- list()
    non_seas_and_reg_deter_comps <- deterministic_components[!grepl(pattern = "seasonal|reg\\d+", x = deterministic_components)]

    mod_list$poly <- dlm::dlmModPoly(
      order = order,
      dV = exp(parm[1]),
      dW = {
        out <- purrr::imap_chr(non_seas_and_reg_state_comps, function(state, i){
          ifelse(state %in% non_seas_and_reg_deter_comps, "0", glue::glue("exp(parm[{i+1}])"))
        }) %>%
          str2expression()
        purrr::map_dbl(out, ~ eval(.x))
      }
    )

    if("seasonal" %in% state_components){

      seas_comp_i <- which(state_components == "seasonal")

      if("seasonal" %in% deterministic_components){
        dW <- rep(0, seasonal_frequency - 1)
      } else {
        dW <- c(
          eval(str2expression(glue::glue("exp(parm[{seas_comp_i+1}])"))),
          rep(0, seasonal_frequency - 2)
        )
      }

      mod_list$seas <- dlm::dlmModSeas(
        frequency = seasonal_frequency,
        dV = 0,
        dW = dW
      )
    }



    if("regressor" %in% state_components){

      reg_comps <- paste0("reg", seq_len(ncol(reg_data)))
      reg_deter_comps <- grep(pattern = "^reg\\d+$", x = deterministic_components, value = TRUE)

      out <- purrr::imap_chr(reg_comps, function(reg_comp, i){
        ifelse(reg_comp %in% reg_deter_comps, "0", glue::glue("exp(parm[{i}])"))
      }) %>%
        str2expression()

      dW <- purrr::map_dbl(out, ~ eval(.x))

      mod_list$reg <- dlm::dlmModReg(
        X = reg_data,
        dV = 0,
        dW = dW,
        addInt = FALSE
      )
    }

    Reduce("+", mod_list)
  }

  # Estimation of the model's hyperparameters with Maximul Likelihood Estimation (MLE)
  hyper_parms_mle_est <- dlm::dlmMLE(
    y = series,
    parm = log(hyper_params_initial),
    build = func_dlm_mod
  )

  # Do we have convergence?
  stopifnot("No convergence was reached on the maximum likelihood estimation of the model's hyperparameters!" = hyper_parms_mle_est$convergence == 0)

  # Build the model using the MLE hyperparameter estimates
  dlm_mod <- func_dlm_mod(parm = hyper_parms_mle_est$par)

  # Filter and smooth the state with the Kalman procedures
  dlm_filtered <- dlm::dlmFilter(y = series, mod = dlm_mod)
  dlm_smoothed <- dlm::dlmSmooth(y = series, mod = dlm_mod)

  # Get log-likelihood at convergence (needed to compute the AIC)
  ll <- get_log_likelihood(dlm_filtered = dlm_filtered)

  # Get log-likelihood at convergence (similar to the book's)
  ll2 <- ll / length(series)

  # Compute model's AIC
  dlm_aic <- get_AIC(log_likelihood = ll, n_state_vars = n_state_vars, n_hyper_params = n_hyper_params)

  # Compute model's AIC (similar to the book's)
  dlm_aic2 <- dlm_aic / length(series)


  # Data frame of all smooth estimates

  smoothed_estimates <- tibble::as_tibble(dlm_smoothed$s, .name_repair = "unique") %>%
    setNames(Reduce(f = c, x = col_names)) %>%
    dplyr::mutate(
      t = dplyr::row_number() - 1,
      series = c(NA, dlm_filtered$y)
    ) %>%
    dplyr::relocate(t, series, 1)

  smoothed_estimates$level_plus_state <- purrr::map_dfc(state_components, function(comp){
    if(comp == "level"){
      out <- smoothed_estimates[["level"]][-1]
    }
    if(comp == "slope"){
      out <- smoothed_estimates[["slope"]][-1]
    }
    if(comp == "regressor"){
      if(is.atomic(reg_data)) reg_data_df <- as.data.frame(reg_data)
      if(!is.atomic(reg_data)) reg_data_df <- reg_data
      reg_colnames <- grep(pattern = "reg\\d+", x = colnames(smoothed_estimates), value = TRUE)
      out <- smoothed_estimates[-1, reg_colnames] * reg_data_df
    }
    if(comp == "seasonal"){
      out <- smoothed_estimates[["seas1"]][-1]
    }
    out
  }) %>%
    rowSums() %>%
    c(NA, .)

  smoothed_estimates$residuals <- c(NA, series) - smoothed_estimates$level_plus_state
  smoothed_estimates$residuals_raw <- c(NA, dlm:::residuals.dlmFiltered(object = dlm_filtered, type = "raw", sd = FALSE))
  smoothed_estimates$residuals_stdzd <- c(NA, dlm:::residuals.dlmFiltered(object = dlm_filtered, type = "standardized", sd = FALSE))

  out <- list(
    series = series,
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

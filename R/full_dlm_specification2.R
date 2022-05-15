full_dlm_modeling <- function(
  series = uk_ksi$log_value,
  state_components = NULL,
  deterministic_components = NULL,
  seasonal_frequency = NULL,
  reg_data = NULL
){

  # ERROR HANDLING HERE!!!

  stopifnot("The 'state_components' argument suggests that the model contains at least one explanatory variable, but no data was provided in the 'reg_data' argument." = "regressor" %in% state_components & is.null(reg_data))

  stopifnot("The 'reg_data' argument must be of one of the following classes: 'numeric', 'matrix' or 'data.frame'." = ("regressor" %in% state_components & class(reg_data)[1] %in% c("matrix", "numeric", "double", "data.frame")) | (!"regressor" %in% state_components & is.null(reg_data)))

  stopifnot("The state_components argument must contain any combination of 'level' (mandatory), 'slope', 'seasonal' and 'regressor'." =  all(state_components %in% c("level", "slope", "seasonal", "regressor")))

  # Order of the polynomial model

  order <- dplyr::case_when(
    !"slope" %in% state_components ~ 1,
    "slope" %in% state_components ~ 2
  )

  # Transform regression data from atomic vector to matrix

  if("regressor" %in% state_components & is.atomic(reg_data)) reg_data <- as.matrix(reg_data)

  # Calculate number of state variables

  n_seasonality <- ifelse("seasonal" %in% state_components, seasonal_frequency - 1, 0)
  n_regressors <- ifelse("regressor" %in% state_components, ncol(reg_data), 0)
  level_and_or_slope_comps <- grep(pattern = "level|slope", x = state_components, value = TRUE)
  n_state_vars <- length(level_and_or_slope_comps) + n_seasonality + n_regressors

  # Stochastic hyperparameters




  # Function for building the model

  func_dlm_mod <- function(parm){

    mod_list <- list()

    mod_list$poly <- dlm::dlmModPoly(
      order = order,
      dV = exp(parm[1]),
      dW = {
        level_and_or_slope_deter_comps <- grep(pattern = "level|slope", x = deterministic_components, value = TRUE)
        out <- purrr::imap_chr(level_and_or_slope_comps, function(comp, i){
          ifelse(comp %in% level_and_or_slope_deter_comps, "0", glue::glue("exp(parm[{1+i}])"))
        }) %>%
          str2expression()
        purrr::map_dbl(out, ~ eval(.x))
      }
    )

    purrr::map(state_components, function(comp){



    })

    # > Level + slope

    dW <-

    mod_list$poly <- dlm::dlmModPoly(
      order = order,
      dV = exp(parm[1])
    )


  }

}

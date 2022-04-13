# UK KSI (Killed or Severely Injured) - from Jan 01, 1969 to Dec 01, 1984

uk_ksi_raw <- readLines(con = here::here("data-raw/UKdriversKSI.txt"))[-1]
uk_ksi <- data.frame(
  date = seq(from = as.Date("1969-01-01"), by = "month", length.out = length(uk_ksi_raw)),
  flow = as.numeric(uk_ksi_raw)
)
uk_ksi$log_flow <- log(uk_ksi$flow)

# Build the local level model for analyzing the data

# > Initial value of the state variance (hyperparameter) (which we want to estimate)
initial_value <- var(uk_ksi$log_flow)

# > Function we use for the MLE estimation
func_llm1_model <- function(parm){
  dlm::dlmModPoly(
    order = 1, # Order 1: Local level model
    dV = exp(parm), # dV: observation variance
    dW = 0, # state variance = 0 (constant mean model)
  )
}

# > MLE estimation of the hyperparameter
llm1_mle_est <- dlm::dlmMLE(
  y = uk_ksi$log_flow, # the log series
  parm = log(initial_value), # the log of the initial value
  build = func_llm1_model, # the function for constructing the model
)

# > We need the convergence to be zero
llm1_mle_est$convergence

# > Build the model with the estimated hyperparameter
llm1_mod <- func_llm1_model(parm = llm1_mle_est$par)

# > Apply the Kalman filter
llm1_filtered <- dlm::dlmFilter(y = uk_ksi$log_flow, mod = llm1_mod)

# > Apply the Kalman smoother
llm1_smoothed <- dlm::dlmSmooth(y = uk_ksi$log_flow, mod = llm1_mod)



usethis::use_data(DATASET, overwrite = TRUE)

# # Source function ----
#
# # source(file = here::here("R/full_dlm_specification.R"))
# # source(file = here::here("R/log_likelihood-and-aic.R"))
#
# # Get the data ----
#
# # > UK KSI
# uk_ksi_raw <- readr::read_lines(file = here::here("data-raw/UKdriversKSI.txt"), skip = 1)
#
# uk_ksi <- tibble::tibble(
#   date = seq(from = as.Date("1969-01-01"), by = "month", length.out = length(uk_ksi_raw)),
#   value = as.numeric(uk_ksi_raw),
#   log_value = log(value)
# )
#
# # > UK gas price
# gas_price_data <- readr::read_lines(file = here::here("data-raw/logUKpetrolprice.txt"), skip = 1) |>
#   as.numeric()
#
# # Seat belt law dummy
# seat_belt_dummy <- rep(0, nrow(ukksi))
# seat_belt_dummy[170:length(seat_belt_dummy)] <- 1
#
# # Chapter 2: The local level model ----
#
# # 2.1 - Deterministic level
#
# (llm1 <- full_dlm_modeling(
#   data = ukksi,
#   t_var = "date",
#   y_var = "log_ksi",
#   state_components = "level",
#   deterministic_components = "level"
# ))
#
# # 2.2 - Stochastic level
#
# (llm2 <- full_dlm_modeling(
#   series = uk_ksi$log_value,
#   state_components = "level",
#   deterministic_components = NULL
# ))
#
# # 2.3 - The local level model and Norwegian fatalities
#
# # ...
#
# # Chapter 3: The local linear trend model ----
#
# # 3.1 Deterministic level and slope
#
# (lltm1 <- full_dlm_modeling(
#   series = uk_ksi$log_value,
#   state_components = c("level", "slope"),
#   deterministic_components = c("level", "slope")
# ))
#
# # 3.2 Stochastic level and slope
#
# (lltm2 <- full_dlm_modeling(
#   series = uk_ksi$log_value,
#   state_components = c("level", "slope"),
#   deterministic_components = NULL
# ))
#
# # 3.3 Stochastic level and deterministic slope
#
# (lltm3 <- full_dlm_modeling(
#   series = uk_ksi$log_value,
#   state_components = c("level", "slope"),
#   deterministic_components = "slope"
# ))
#
# # 3.4 The local linear trend model and Finnish fatalities
#
# # ...
#
# # Chapter 4: The local level model with seasonal ----
#
# # 4.1 Deterministic level and seasonal
#
# (llms1 <- full_dlm_modeling(
#   series = uk_ksi$log_value,
#   state_components = c("level", "seasonal"),
#   deterministic_components = c("level", "seasonal"),
#   seasonal_frequency = 12
# ))
#
# # 4.2 Stochastic level and seasonal
#
# (llms2 <- full_dlm_modeling(
#   series = uk_ksi$log_value,
#   state_components = c("level", "seasonal"),
#   deterministic_components = NULL,
#   seasonal_frequency = 12
# ))
#
# # 4.3 Stochastic level and deterministic seasonal
#
# (llms3 <- full_dlm_modeling(
#   series = uk_ksi$log_value,
#   state_components = c("level", "seasonal"),
#   deterministic_components = "seasonal",
#   seasonal_frequency = 12
# ))
#
# # 4.4 The local level and seasonal model and UK inflation
#
# # ...
#
# # Chapter 5 - The local level model with explanatory variable
#
# # 5.1a Deterministic level and explanatory variable
#
# (llmev1 <- full_dlm_modeling(
#   series = uk_ksi$log_value,
#   state_components = c("level", "regressor"),
#   deterministic_components = c("level", "reg1"),
#   reg_data = as.numeric(seq_len(nrow(uk_ksi)))
# ))
#
# # 5.1b Deterministic level and deterministic explanatory variable
#
# (llmev1b <- full_dlm_modeling(
#   series = uk_ksi$log_value,
#   state_components = c("level", "regressor"),
#   deterministic_components = c("level", "reg1"),
#   reg_data = gas_price_data
# ))
#
# # 5.2 Stochastic level and deterministic explanatory variable
#
# (llmev2 <- full_dlm_modeling(
#   series = uk_ksi$log_value,
#   state_components = c("level", "regressor"),
#   deterministic_components = "reg1",
#   reg_data = gas_price_data
# ))
#
#
# # Chapter 6 - The local level model with intervention variable
#
# # 6.1 Deterministic level and deterministic intervention variable
#
# (llmiv1 <- full_dlm_modeling(
#   series = uk_ksi$log_value,
#   state_components = c("level", "regressor"),
#   deterministic_components = c("level", "reg1"),
#   reg_data = seat_belt_dummy
# ))
#
# # 6.2 Stochastic level and deterministic intervention variable
#
# new_mod <- full_dlm_modeling(
#   data = ukksi,
#   y_var = "log_ksi",
#   t_var = "date",
#   state_components = c("level", "regressor"),
#   deterministic_components = "reg1",
#   reg_vars = "sb_law"
# )
#
# (llmiv2 <- full_dlm_modeling(
#   series = uk_ksi$log_value,
#   time_variable = uk_ksi$date,
#   state_components = c("level", "regressor"),
#   deterministic_components = "reg1",
#   reg_data = seat_belt_dummy
# ))
#
# dlm_mod <- llmiv2
#
# # Chapter 0 - Random models I am just trying out ----
#
# # 0.1 Local linear trend model with seasonal and two explanatory variables
#
# mod1 <- full_dlm_modeling(
#   series = log(ukksi$value),
#   state_components = c("level", "slope", "seasonal", "regressor"),
#   deterministic_components = NULL,
#   seasonal_frequency = 12,
#   reg_data = data.frame(
#     time = as.numeric(seq_len(nrow(ukksi))),
#     gas_price_data = gas_price_data
#   )
# )
#
# dlm_mod <- mod1
#
#
# # initial_values <- c(
# #   var(uk_ksi$log_value),
# #   rep(0.001, 2)
# # )
# #
# # build_func <- function(parm){
# #   mod1 <- dlm::dlmModPoly(
# #     order = 1,
# #     dV = exp(parm[1]),
# #     dW = exp(parm[2])
# #   )
# #   mod2 <- dlm::dlmModReg(
# #     X = gas_price_data,
# #     dV = 0,
# #     dW = exp(parm[3]),
# #     addInt = FALSE
# #   )
# #   mod1 + mod2
# # }
# #
# # mle_est <- dlm::dlmMLE(
# #   y = uk_ksi$log_value,
# #   parm = log(initial_values),
# #   build = build_func
# # )
# #
# # mod <- build_func(parm = mle_est$par)
# #
# # filt <- dlm::dlmFilter(y = uk_ksi$log_value, mod = mod)
# # smot <- dlm::dlmSmooth(y = filt, mod = mod)
# #
# # smot$s[2,1]

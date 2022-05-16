# Source function ----

source(file = here::here("R/full_dlm_specification2.R"))
source(file = here::here("R/log_likelihood-and-aic.R"))

# Get the data ----

# > UK KSI
uk_ksi_raw <- readr::read_lines(file = here::here("data-raw/UKdriversKSI.txt"), skip = 1)

uk_ksi <- tibble::tibble(
  date = seq(from = as.Date("1969-01-01"), by = "month", length.out = length(uk_ksi_raw)),
  value = as.numeric(uk_ksi_raw),
  log_value = log(value)
)

# > UK gas price
gas_price_data <- readr::read_lines(file = here::here("data-raw/logUKpetrolprice.txt"), skip = 1) |>
  as.numeric()


# Chapter 2: The local level model ----

# 2.1 - Deterministic level

(llm1 <- full_dlm_modeling(
  series = uk_ksi$log_value,
  state_components = "level",
  deterministic_components = "level"
))

# 2.2 - Stochastic level

(llm2 <- full_dlm_modeling(
  series = uk_ksi$log_value,
  state_components = "level",
  deterministic_components = NULL
))

# 2.3 - The local level model and Norwegian fatalities

# ...

# Chapter 3: The local linear trend model ----

# 3.1 Deterministic level and slope

(lltm1 <- full_dlm_modeling(
  series = uk_ksi$log_value,
  state_components = c("level", "slope"),
  deterministic_components = c("level", "slope")
))

# 3.2 Stochastic level and slope

(lltm2 <- full_dlm_modeling(
  series = uk_ksi$log_value,
  state_components = c("level", "slope"),
  deterministic_components = NULL
))

# 3.3 Stochastic level and deterministic slope

(lltm3 <- full_dlm_modeling(
  series = uk_ksi$log_value,
  state_components = c("level", "slope"),
  deterministic_components = "slope"
))

# 3.4 The local linear trend model and Finnish fatalities

# ...

# Chapter 4: The local level model with seasonal ----

# 4.1 Deterministic level and seasonal

(llms1 <- full_dlm_modeling(
  series = uk_ksi$log_value,
  state_components = c("level", "seasonal"),
  deterministic_components = c("level", "seasonal"),
  seasonal_frequency = 12
))

# 4.2 Stochastic level and seasonal

(llms2 <- full_dlm_modeling(
  series = uk_ksi$log_value,
  state_components = c("level", "seasonal"),
  deterministic_components = NULL,
  seasonal_frequency = 12
))

# 4.3 Stochastic level and deterministic seasonal

(llms3 <- full_dlm_modeling(
  series = uk_ksi$log_value,
  state_components = c("level", "seasonal"),
  deterministic_components = "seasonal",
  seasonal_frequency = 12
))

# 4.4 The local level and seasonal model and UK inflation

# ...

# Chapter 5 - The local level model with explanatory variable

# 5.1a Deterministic level and explanatory variable

(llmev1 <- full_dlm_modeling(
  series = uk_ksi$log_value,
  state_components = c("level", "regressor"),
  deterministic_components = c("level", "reg1"),
  reg_data = as.numeric(seq_len(nrow(uk_ksi)))
))

# 5.1b Deterministic level and explanatory variable

(llmev1b <- full_dlm_modeling(
  series = uk_ksi$log_value,
  state_components = c("level", "regressor"),
  deterministic_components = c("level", "reg1"),
  reg_data = gas_price_data
))

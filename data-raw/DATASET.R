# UK drivers KSI (killed or seriously injured)

uk_ksi_raw <- readr::read_lines(file = here::here("data-raw/UKdriversKSI.txt"), skip = 1)
gas_price_data <- readr::read_lines(file = here::here("data-raw/logUKpetrolprice.txt"), skip = 1) |>
    as.numeric()

ukksi <- dplyr::tibble(
  date = seq(from = as.Date("1969-01-01"), by = "month", length.out = length(uk_ksi_raw)),
  ksi = as.numeric(uk_ksi_raw),
  log_ksi = log(ksi),
  gas = exp(gas_price_data),
  log_gas = gas_price_data,
  sb_law = {
    out <- rep(0, nrow(ukksi))
    out[170:length(out)] <- 1
    out
  }
)

usethis::use_data(ukksi, overwrite = TRUE)

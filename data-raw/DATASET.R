# UK drivers KSI (killed or seriously injured)

uk_ksi_raw <- readr::read_lines(file = here::here("data-raw/UKdriversKSI.txt"), skip = 1)

ukksi <- tibble::tibble(
  date = seq(from = as.Date("1969-01-01"), by = "month", length.out = length(uk_ksi_raw)),
  value = as.numeric(uk_ksi_raw)
)

usethis::use_data(ukksi, overwrite = TRUE)

#' UK Killed or Seriously Injured (KSI)
#'
#' A dataset containing the number of killed or seriously injured (KSI) in the UK from January 1969 to December 1984.
#'
#' @format A data frame/tibble with 192 rows and 2 columns:
#' \describe{
#'    \item{date}{Date column}
#'    \item{ksi}{Number of people killed or seriously injured (KSI)}
#'    \item{log_ksi}{Log of ksi}
#'    \item{gas}{Gas price}
#'    \item{log_gas}{Log of gas price}
#'    \item{sb_law}{Dummy variable for the seat belt law}
#' }
#'@source \url{https://github.com/sinhrks/stan-statespace}
"ukksi"

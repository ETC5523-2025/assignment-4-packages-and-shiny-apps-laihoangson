#' Quarantine Breaches Data
#'
#' Contains individual quarantine breach events in Australia.
#'
#' @format A tibble with 39 rows and 9 columns:
#' \describe{
#'   \item{no}{Record number}
#'   \item{state}{State or territory code}
#'   \item{date}{Date of the breach event}
#'   \item{facility}{Facility name}
#'   \item{case_name}{Role or person involved}
#'   \item{variant}{COVID-19 variant, if known}
#'   \item{onward}{Whether community transmission occurred (TRUE/FALSE)}
#'   \item{vax}{Vaccination status if available}
#'   \item{link}{Reference URL for the report}
#' }
"breaches_data"

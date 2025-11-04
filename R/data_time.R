#' Estimated Quarantine Risk Over Time
#'
#' Daily modelled estimates of quarantine risk by state and nationally.
#'
#' @format A tibble with 1,946 rows and 4 columns:
#' \describe{
#'   \item{report_date}{Date of the risk estimate}
#'   \item{state}{State code (or "AUS" for national total)}
#'   \item{total}{Estimated total quarantine risk}
#'   \item{breach}{Estimated risks associated with quarantine system failures (may be NA)}
#' }
"data_time"

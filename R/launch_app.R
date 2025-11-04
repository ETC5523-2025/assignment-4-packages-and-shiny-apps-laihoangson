#' Launch the Quarantine Shiny Application
#'
#' This function starts the interactive Shiny app included in the package,
#' which allows users to explore the quarantine breach data.
#'
#' @export
#' @importFrom shiny runApp
#' @return This function does not return a value; it is called for its
#'   side effect of launching a Shiny application.
#'
launch_app <- function() {
  app_dir <- system.file("shiny_app", package = "QuarantineAnalysis")

  if (app_dir == "") {
    stop(
      "Could not find the shiny app directory. ",
      "Try reinstalling the `QuarantineAnalysis` package.",
      call. = FALSE
    )
  }

  shiny::runApp(app_dir, display.mode = "normal")
}

#' Launch the spatiAlert Shiny application
#'
#' @description
#' Opens the spatiAlert interactive hotspot analysis tool in your default
#' browser. All data processing happens locally on your machine.
#'
#' @param port Integer. Port to run the app on. Default is a random available port.
#' @param launch.browser Logical. Open in browser automatically? Default TRUE.
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return Invisibly returns NULL. Called for its side effect of launching the app.
#'
#' @examples
#' \dontrun{
#' # Launch the app
#' hotspot_app()
#'
#' # Run on a specific port
#' hotspot_app(port = 3838)
#' }
#'
#' @export
hotspot_app <- function(port = NULL, launch.browser = TRUE, ...) {
  app_dir <- system.file("app", package = "spatialert")
  if (app_dir == "") {
    stop(
      "Could not find the app directory. ",
      "Try re-installing spatialert: remotes::install_github('DMA-PRIME/spatiAlert')",
      call. = FALSE
    )
  }

  # bslib::card()/card_body() are also fully qualified in the app's UI code,
  # but re-attach bslib here too as a second safeguard: if the calling R
  # session already had another package loaded that exports a function of
  # the same name (e.g. spdep::card(), which computes neighbour-list
  # cardinality), attaching bslib last ensures it isn't masked.
  suppressMessages(library(bslib))

  shiny::runApp(
    appDir    = app_dir,
    port      = port,
    launch.browser = launch.browser,
    ...
  )
}

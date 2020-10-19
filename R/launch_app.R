#' Launch shiny app visualizing the spread of COVID-19 across the states of Australia
#' 
#' @return A Shiny Application
#' 
#' @example 
#' launch_app()
#' 
#' @export
launch_app <- function() {
  appDir <- system.file("app", package = "AusCovid19")
  if (appDir == "") {
    stop("Could not launch app. Try re-installing `AusCovid19`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}
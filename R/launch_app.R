#' @export
launch_app <- function() {
  appDir <- system.file("app", package = "AusCovidDashboard")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}
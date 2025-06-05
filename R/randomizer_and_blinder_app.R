#' Randomizer and blinder tool - To be deprecated
#'
#' See the inst/ folder for the main code for this function
#'
#' @export
randblinder_shiny_tool <- function() {
    deprecation_warn("randomizer_and_blinder_app.randblinder_shiny_tool")

    appFile <- system.file("shiny_app/randblinder.R", package = "CIDAtools")
    if (appFile == "") {
      stop("Could not find the Shiny app file. Try re-installing CIDAtools.", call. = FALSE)
    }
    shiny::runApp(appFile, display.mode = "normal")
}

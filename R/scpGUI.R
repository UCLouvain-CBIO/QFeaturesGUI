#' @title A simple graphical interface for the scp package
#'
#' @description scpGUI is a simple graphical interface to handle SCP data powered by the scp package.
#' The app is divided into mutliple sections that represent the different steps of a typical SCP analysis.
#' \itemize{
#' \item The first section (Import) of the app allow to convert two tables (Input and Sample) to a QFeatures object.
#' This process is performed with the \code{\link[scp]{readSCP}} function from the scp package.
#' }
#' @param example A logical value indicating if the app should be launched with an example dataset.
#' @return Return the "scpGUI" shiny app object.
#' @export
#' @importFrom shiny shinyApp runApp
#'
#' @examples
#' library(scpGUI)
#'
#' app <- scpGUI(example = TRUE)
#'
#' if (interactive()) {
#'     shiny::runApp(app)
#' }
#'
scpGUI <- function(example = FALSE) {
    global_rv$example <- example
    ui <- build_ui()
    server <- build_server()
    shinyApp(ui = ui, server = server)
}

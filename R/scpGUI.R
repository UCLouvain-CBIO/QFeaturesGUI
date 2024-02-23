#' @title A simple graphical interface for the scp package
#'
#' @description scpGUI is a simple graphical interface to handle SCP data.
#' The first section (Import) of the app allow to convert two tables (Input and Sample) to a QFeatures object.
#' This process is performed with the \code{\link[scp]{readSCP}} function from the scp package.
#'
#' @return Return the "scpGUI" shiny app object.
#' @export
#' @import shiny
#' @import shinydashboard
#' @import shinydashboardPlus
#' @importFrom SummarizedExperiment assay
#' @import scp
#' @importFrom DT renderDataTable datatable
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
    ui <- .build_ui()
    server <- .build_server()
    shinyApp(ui = ui, server = server)
}

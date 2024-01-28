#' @title A simple graphical interface to import SCP data
#'
#' @description scpGUI is a simple graphical interface to handle SCP data.
#' The first page of the app allow to convert two tables (Input and Sample) to a QFeatures object.
#' This process is performed with the \code{\link[scp]{readSCP}} function from the scp package.
#'
#' @return Return the "scpGUI" shiny app object.
#' @export
#' @import shiny
#' @import shinydashboard
#' @import shinydashboardPlus
#' @importFrom SummarizedExperiment assay
#' @importFrom scp readSCP
#' @importFrom DT renderDataTable datatable
#' @examples
#' library(scpGUI)
#'
#' app <- scpGUI()
#'
#' if (interactive()) {
#'     shiny::runApp(app, ...)
#' }
#'
scpGUI <- function() {
    ui <- .build_ui()
    server <- .build_server()
    app <- shinyApp(ui = ui, server = server)
    return(app)
}

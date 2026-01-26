#' @title A shiny app to import QFeatures objects.
#'
#' @description importQFeatures is a simple graphical interface to import bulk and single-cell proteomic data.
#' The app use the \code{\link[QFeatures]{readQFeatures}} function from the QFeatures package to convert simple table (single or multiple, csv or tsv) to a QFeatures object.
#' The app allow to convert tables to a QFeatures object.
#'
#' @param colData A dataframe that contains the sample table.
#' @param assayData A dataframe that contains the input table.
#' @param maxSize An integer that change shiny.maxRequestSize value, this value has to be in Mb.
#'
#' @return Return the "importQFeatures" shiny app object.
#' @export
#' @importFrom shiny shinyApp runApp
#'
#' @examples
#' library(QFeaturesGUI)
#'
#' data("sampleTable")
#' data("inputTable")
#' app <- importQFeatures(colData = sampleTable, assayData = inputTable, maxSize = 100)
#'
#' if (interactive()) {
#'     shiny::runApp(app)
#' }
#'
importQFeatures <- function(colData = NULL, assayData = NULL, maxSize = 1000) {
    oldOptions <- options(shiny.maxRequestSize = maxSize * 1024^2)
    on.exit(options(oldOptions))
    ui <- build_import_ui()
    server <- build_import_server(colData, assayData)
    shinyApp(ui = ui, server = server)
}

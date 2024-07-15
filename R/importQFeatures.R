#' @title A shiny app to import QFeatures objects, some optional pre-processing steps.
#'
#' @description importQFeatures is a simple graphical interface to import bulk and single-cell proteomic data. 
#' The app use the \code{\link[QFeatures]{readQFeatures}} function from the QFeatures package to convert simple table (single or multiple, csv or tsv) to a QFeatures object.
#' The app is divided into mutliple sections:
#' \itemize{
#' \item The first section (Import) of the app allow to convert tables to a QFeatures object.
#' \item The second section (Pre-processing) of the app allow to perform some optional pre-processing steps.
#' }
#' @param sample_table A dataframe that contains the sample table.
#' @param input_table A dataframe that contains the input table.
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
#' app <- importQFeatures(sample_table = sampleTable, input_table = inputTable)
#'
#' if (interactive()) {
#'     shiny::runApp(app)
#' }
#'
importQFeatures <- function(sample_table = NULL, input_table = NULL) {
    ui <- build_ui()
    server <- build_server(sample_table, input_table)
    shinyApp(ui = ui, server = server)
}

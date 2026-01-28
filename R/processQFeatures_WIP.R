#' @title A shiny app to process QFeatures objects.
#'
#' @description processQFeatures is a simple graphical interface to process QFeatures object.
#' The app is divided into mutliple sections:
#' \itemize{
#' \item The first section (Import) of the app allow to convert tables to a QFeatures object.
#' \item The second section (Pre-processing) of the app allow to perform some optional pre-processing steps.
#' }
#' @param colData A dataframe that contains the sample table.
#' @param assayData A dataframe that contains the input table.
#'
#' @return Return the "importQFeatures" shiny app object.
#' @export
#' @importFrom shiny shinyApp runApp addResourcePath
#'
#' @examples
#' library(QFeaturesGUI)
#'
#' data("sampleTable")
#' data("inputTable")
#' app <- importQFeatures(colData = sampleTable, assayData = inputTable)
#'
#' if (interactive()) {
#'     shiny::runApp(app)
#' }
#'
processQFeatures_WIP <- function(colData = NULL, assayData = NULL) {
    options(shiny.maxRequestSize = 100 * 1024^2)
    addResourcePath(
        "app-assets",
        system.file("www", package = "QFeaturesGUI")
    )
    ui <- build_ui()
    server <- build_server(colData, assayData)
    shinyApp(ui = ui, server = server)
}

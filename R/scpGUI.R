#' @title A simple graphical interface for the scp package
#'
#' @description scpGUI is a simple graphical interface to handle SCP data powered by the scp package.
#' The app is divided into mutliple sections that represent the different steps of a typical SCP analysis.
#' \itemize{
#' \item The first section (Import) of the app allow to convert two tables (Input and Sample) to a QFeatures object.
#' This process is performed with the \code{\link[scp]{readSCP}} function from the scp package.
#' }
#' @param sample_table A dataframe that contains the sample table.
#' @param input_table A dataframe that contains the input table.
#'
#' @return Return the "scpGUI" shiny app object.
#' @export
#' @importFrom shiny shinyApp runApp
#'
#' @examples
#' library(scpGUI)
#'
#' data("sampleTable")
#' data("inputTable")
#' app <- scpGUI(sample_table = sampleTable, input_table = inputTable)
#'
#' if (interactive()) {
#'     shiny::runApp(app)
#' }
#'
scpGUI <- function(sample_table = NULL, input_table = NULL) {
    ui <- build_ui()
    server <- build_server(sample_table, input_table)
    shinyApp(ui = ui, server = server)
}

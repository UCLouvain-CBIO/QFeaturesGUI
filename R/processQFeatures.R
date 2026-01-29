#' @title A shiny app to process QFeatures objects.
#'
#' @description processQFeatures is a simple graphical interface to process QFeatures object.
#'
#' @param qfeatures a `QFeatures` object that will be process by the application
#'  it can also be the path to a .rds file that contains a `QFeatures`
#' @param initialSets numeric() specifying which sets to use as starting point
#'   for the processing
#'
#' @return Return the "processQFeatures" shiny app object.
#' @export
#' @importFrom shiny shinyApp runApp addResourcePath
#'
#' @examples
#'
#' library(QFeatures)
#' library(QFeaturesGUI)
#'
#' data("sampleTable")
#' data("inputTable")
#' qfeatures <- readQFeatures(inputTable,
#'     colData = sampleTable,
#'     runCol = "Raw.file"
#' )
#' app <- processQFeatures(qfeatures, initialSets = seq_along(qfeatures))
#'
#' if (interactive()) {
#'     shiny::runApp(app)
#' }
#'
processQFeatures <- function(qfeatures, initialSets = seq_along(qfeatures)) {
    if (missing(qfeatures)) stop("`qfeatures` argument is missing")
    # If qfeatures is a character, treat it as a path
    if (is.character(qfeatures)) {
        if (!file.exists(qfeatures)) {
            stop("The file '", qfeatures, "' does not exist.")
        }
        loaded <- tryCatch(readRDS(qfeatures),
            error = function(e) {
                stop("Failed to read RDS file: ", e$message)
            }
        )
        qfeatures <- loaded
    }
    if (!inherits(qfeatures, "QFeatures")) {
        stop("`qfeatures` must be a QFeatures object or a valid path to an RDS file containing one.")
    }

    options(shiny.maxRequestSize = 100 * 1024^2)
    addResourcePath(
        "app-assets",
        system.file("www", package = "QFeaturesGUI")
    )
    ui <- build_process_ui()
    server <- build_process_server(qfeatures, initialSets)
    shinyApp(ui = ui, server = server)
}


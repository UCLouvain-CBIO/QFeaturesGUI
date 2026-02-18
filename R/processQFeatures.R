#' Launch a Shiny application to process QFeatures objects
#'
#' @description
#' \code{processQFeatures()} launches an interactive Shiny application
#' that allows users to visually configure and apply pre-processing
#' workflows to a \linkS4class{QFeatures} object.
#'
#' The input \code{qfeatures} can be provided either as an in-memory
#' \linkS4class{QFeatures} object or as a path to an \code{.rds} file
#' containing one.
#'
#' @param qfeatures A \linkS4class{QFeatures} object to be processed,
#'   or a character string specifying the path to a \code{.rds} file
#'   containing a \linkS4class{QFeatures} object.
#'
#' @param initialSets An integer, logical, or character vector specifying
#'   which assays (feature sets) should be used as the starting point for
#'   processing. Defaults to all assays in \code{qfeatures}.
#'
#' @param prefilledSteps A character vector specifying the initial workflow
#'   steps to display when the application launches. Steps must be provided
#'   using their internal identifiers (e.g. \code{"sample_filtering"},
#'   \code{"feature_filtering"}, \code{"normalisation"}).
#'
#' @return
#' The processQFeatures shiny application.
#'
#' @details
#' The application provides a drag-and-drop workflow builder that allows
#' users to select, order, and configure processing steps such as filtering,
#' normalization, and transformation. The configured workflow can then be
#' applied to the selected assays.
#'
#' @export
#'
#' @importFrom shiny shinyApp runApp addResourcePath
#'
#' @examples
#' library(QFeatures)
#' library(QFeaturesGUI)
#'
#' data("sampleTable")
#' data("inputTable")
#'
#' qfeatures <- readQFeatures(
#'     inputTable,
#'     colData = sampleTable,
#'     runCol = "Raw.file"
#' )
#'
#' app <- processQFeatures(
#'     qfeatures,
#'     initialSets = seq_along(qfeatures)
#' )
#'
#' if (interactive()) {
#'     shiny::runApp(app)
#' }
processQFeatures <- function(
      qfeatures,
      initialSets = seq_along(qfeatures),
      prefilledSteps = c(
          "sample_filtering",
          "feature_filtering"
      )
) {
    ## Validate QFeatures input
    qfeatures <- check_qfeatures(qfeatures)

    ## Normalize initial assay selection
    initial_sets <- normalise_initial_sets(qfeatures, initialSets)

    ## Validate and map workflow steps
    initial_steps <- check_prefilled_steps(prefilledSteps)

    options(shiny.maxRequestSize = 100 * 1024^2)
    addResourcePath(
        "app-assets",
        system.file("www", package = "QFeaturesGUI")
    )

    ui <- build_process_ui(initial_steps)
    server <- build_process_server(qfeatures, initial_sets, initial_steps)

    shinyApp(ui = ui, server = server)
}

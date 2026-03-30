#' UI builder for import app
#'
#' @return A shiny dashboard UI for importQFeatures app
#' @rdname INTERNAL_build_import_ui
#' @keywords internal
#'
#' @importFrom shinydashboard dashboardBody tabItem tabItems 
#' @importFrom shinydashboardPlus dashboardPage dashboardSidebar
#' @importFrom htmltools includeCSS
#' @importFrom shinyFeedback useShinyFeedback
#' @importFrom shinyjs useShinyjs
#'
build_import_ui <- function() {
    ui <- dashboardPage(
        skin = "blue",
        header = header("import QFeatures App"),
        dashboardSidebar(disable = TRUE, minified = FALSE, width = 0),
        dashboardBody(
            useShinyjs(),
            includeCSS(system.file(package = "QFeaturesGUI", "www", "style.css")),
            import_tab()
        ),
        scrollToTop = TRUE
    )
    return(ui)
}

#' UI builder for import app
#'
#' @return A shiny dashboard UI for importQFeatures app
#' @rdname INTERNAL_build_import_ui
#' @keywords internal
#'
#' @importFrom shinydashboard dashboardBody tabItem tabItems dashboardSidebar
#' @importFrom shinydashboardPlus dashboardPage
#' @importFrom htmltools includeCSS
#' @importFrom shinyFeedback useShinyFeedback
#' @importFrom shinyjs useShinyjs
#'
build_import_ui <- function() {
    ui <- dashboardPage(
        skin = "blue",
        header = header("import QFeatures App"),
        dashboardSidebar(collapsed = TRUE, disable = TRUE, width = 0),
        dashboardBody(
            useShinyjs(),
            includeCSS(system.file(package = "QFeaturesGUI", "www", "style.css")),
            import_tab()
        )
    )
    return(ui)
}

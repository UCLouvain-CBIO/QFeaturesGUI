#' UI builder for import app
#'
#' @return A shiny dashboard UI for importQFeatures app
#' @rdname INTERNAL_build_import_ui
#' @keywords internal
#'
#' @importFrom shinydashboard dashboardBody tabItem tabItems
#' @importFrom shinydashboardPlus dashboardPage
#' @importFrom htmltools includeCSS
#' @importFrom shinyFeedback useShinyFeedback
#'
build_import_ui <- function() {
    ui <- dashboardPage(
        skin = "blue",
        header = header("import QFeatures App"),
        dashboardSidebar(collapsed = TRUE, disable = TRUE, width = 0),
        dashboardBody(
            import_tab()
        )
    )
    return(ui)
}

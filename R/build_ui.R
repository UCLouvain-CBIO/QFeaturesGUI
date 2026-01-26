#' UI builder
#'
#' @return A shiny dashboard UI
#' @rdname INTERNAL_build_ui
#' @keywords internal
#'
#' @importFrom shinydashboard dashboardBody tabItem tabItems
#' @importFrom shinydashboardPlus dashboardPage
#' @importFrom htmltools includeCSS
#' @importFrom shinyFeedback useShinyFeedback
#'
build_ui <- function() {
    ui <- dashboardPage(
        skin = "blue",
        header = header("Process QFeatures"),
        sidebar = sidebar(),
        body = dashboardBody(
            useShinyFeedback(),
            includeCSS(system.file(package = "QFeaturesGUI", "www", "style.css")),
            uiOutput("all_tabs")
        ),
        title = "importQFeatures"
    )

    return(ui)
}

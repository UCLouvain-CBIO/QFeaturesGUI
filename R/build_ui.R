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
        header = header(),
        sidebar = sidebar(),
        body = dashboardBody(
            useShinyFeedback(),
            includeCSS(system.file(package = "scpGUI", "www", "style.css")),
            uiOutput("all_tabs")
        ),
        title = "scpGUI"
    )

    return(ui)
}

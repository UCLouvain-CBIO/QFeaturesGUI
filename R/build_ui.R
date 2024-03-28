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
            tabItems(
                tabItem(
                    tabName = "import_tab",
                    import_tab()
                ),
                tabItem(
                    tabName = "features_filtering_tab",
                    interface_module_features_filtering_tab("PSM1")
                ),
                tabItem(
                    tabName = "cell_filtering_tab",
                    "WIP"
                ),
                tabItem(
                    tabName = "psm_na_report_tab",
                    "WIP"
                ),
                tabItem(
                    tabName = "peptides_na_report_tab",
                    "WIP"
                )
            )
        ),
        title = "scpGUI"
    )

    return(ui)
}

#' sidebar builder for the scpGUI app
#'
#' @return return a dashboardSidebar object that contains the sidebar menu
#' that contains the tabNames
#' @rdname INTERNAL_interface_sidebar
#' @keywords internal
#'
#' @importFrom shiny icon
#' @importFrom shinydashboard dashboardSidebar sidebarMenu menuItem menuSubItem menuItemOutput
#'
sidebar <- function() {
    dashboardSidebar(
        sidebarMenu(
            id = "sidebar_menu",
            menuItem(
                "Import",
                tabName = "import_tab",
                icon = shiny::icon("1") # , class = "icon-container")
            ),
            menuItem(
                "Pre-processing Configuration",
                tabName = "workflow_config_tab",
                icon = shiny::icon("2")
            ),
            menuItemOutput("sidebar_workflow"),
            menuItem(
                "Summary",
                tabName = "summary_tab",
                icon = shiny::icon("4")
            )
        )
    )
}

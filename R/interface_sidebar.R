#' sidebar builder for the scpGUI app
#'
#' @return return a dashboardSidebar object that contains the sidebar menu
#' that contains the tabNames
#' @rdname INTERNAL_interface_sidebar
#' @keywords internal
#'
#' @importFrom shiny icon
#' @importFrom shinydashboard dashboardSidebar sidebarMenu menuItem
#'
sidebar <- function() {
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                "Import",
                tabName = "import_tab",
                icon = shiny::icon("1") # , class = "icon-container")
            ),
            menuItem(
                "Quality Control",
                tabName = "qc_tab",
                icon = shiny::icon("2")
            )
        )
    )
}

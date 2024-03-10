#' sidebar builder for the scpGUI app
#'
#' @return return a dashboardSidebar object that contains the sidebar menu
#' that contains the tabNames
#' @rdname INTERNAL_interface_sidebar
#' @keywords internal
#'
#' @importFrom shiny icon
#' @importFrom shinydashboard dashboardSidebar sidebarMenu menuItem menuSubItem
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
                icon = shiny::icon("2"),
                startExpanded = FALSE,
                menuSubItem(
                    "PSM Filtering",
                    tabName = "features_filtering_tab"
                ),
                menuSubItem(
                    "Cells Filtering",
                    tabName = "cell_filtering_tab"
                )
            ),
            menuItem(
                "NA report",
                icon = shiny::icon("3"),
                startExpanded = FALSE,
                menuSubItem(
                    "PSM NA report",
                    tabName = "psm_na_report_tab"
                ),
                menuSubItem(
                    "Peptides NA report",
                    tabName = "peptides_na_report_tab"
                )
            )
        )
    )
}

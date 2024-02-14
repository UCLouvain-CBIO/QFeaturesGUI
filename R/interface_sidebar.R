sidebar <- function() {
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                "Import",
                tabName = "import_tab",
                icon = shiny::icon("check")
            ),
            menuItem(
                "Quality Control",
                tabName = "qc_tab",
                icon = shiny::icon("check")
            )
        )
    )
}

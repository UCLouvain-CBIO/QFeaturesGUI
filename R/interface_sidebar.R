sidebar <- function() {
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                "Import",
                tabName = "import_tab",
                icon = shiny::icon("1") #, class = "icon-container")
            ),
            menuItem(
                "Quality Control",
                tabName = "qc_tab",
                icon = shiny::icon("2")
            )
        )
    )
}

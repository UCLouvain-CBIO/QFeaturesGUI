.build_ui <- function() {
    ui <- dashboardPage(
        header = header(),
        sidebar = sidebar(),
        body = dashboardBody(
            includeCSS("inst/www/style.css"),
            waiter::autoWaiter(),
            tabItems(
                tabItem(
                    tabName = "import_tab",
                    import_page()
                ),
                tabItem(
                    tabName = "qc_tab",
                    qc_page()
                )
            )
        ),
        title = "scpGUI"
    )

    return(ui)
}

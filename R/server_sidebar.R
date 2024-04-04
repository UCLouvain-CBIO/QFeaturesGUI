server_sidebar <- function(input, output, session) {
    output$sidebar_workflow <- renderMenu({
        menuItem(
            paste0(
                "Workflow",
                " (",
                length(global_rv$workflow_config), " Steps)"
            ),
            tabName = "workflow_tab",
            icon = shiny::icon("3"),
            lapply(seq_along(global_rv$workflow_config), function(index) {
                menuSubItem(
                    text = global_rv$workflow_config[[index]],
                    tabName = paste0("step_", index)
                )
            })
        )
    })
}

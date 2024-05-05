#' Server sidebar logic
#'
#' @param input input from the parent server function
#' @param output output from the parent server function
#' @param session session from the parent server function
#'
#' @return The server logic for the sidebar
#' @rdname INTERNAL_server_sidebar
#' @keywords internal
#'
#' @importFrom shinydashboard renderMenu menuItem menuSubItem
#' @importFrom shiny icon
#'
server_sidebar <- function(input, output, session) {
    output$sidebar_workflow <- renderMenu({
        menuItem(
            paste0(
                "Pre-processing",
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

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
    output$dynamic_sidebar <- renderMenu({
        n_steps <- length(global_rv$workflow_config)

        menu_list <- list()
        if (n_steps > 0) {
            step_items <- lapply(seq_len(n_steps), function(i) {
                menuSubItem(
                    text = global_rv$workflow_config[[i]],
                    tabName = paste0("step_", i),
                    icon = icon(as.character(i))
                )
            })
        } else {
            step_items <- 0
        }

        menuItem(
            paste0("Pre-processing (", n_steps, " steps)"),
            icon = icon("list-check"),
            # list-ol
            step_items
        )
    })
}

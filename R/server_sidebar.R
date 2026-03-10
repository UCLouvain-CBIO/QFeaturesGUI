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
        step_rvs <- global_rv$step_rvs

        if (n_steps > 0) {
            step_items <- lapply(seq_len(n_steps), function(i) {
                is_available <- i == 1 ||
                    (!is.null(step_rvs) &&
                        length(step_rvs) >= (i - 1) &&
                        step_rvs[[i - 1]]() > 0L)
                is_saved <- !is.null(step_rvs) &&
                    length(step_rvs) >= i &&
                    step_rvs[[i]]() > 0L

                step_icon <- if (!is_available) {
                    icon("lock", style = "color: #aaa;")
                } else if (is_saved) {
                    icon("check", style = "color: #00a65a;")
                } else {
                    icon("clock", style = "color: #f39c12;")
                }

                menuSubItem(
                    text = paste0("Step ", i, ": ", global_rv$workflow_config[[i]]),
                    tabName = paste0("step_", i),
                    icon = step_icon
                )
            })
        } else {
            step_items <- NULL
        }

        menuItem(
            paste0("Pre-processing (", n_steps, " steps)        "),
            icon = icon("list-check"),
            startExpanded = TRUE,
            step_items
        )
    })
}

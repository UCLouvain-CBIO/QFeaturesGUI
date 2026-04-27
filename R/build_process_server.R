#' Server Builder for the processQFeatures app
#'
#' @param qfeatures a `QFeatures` object given by the user
#' @param initial_sets index of the base sets of the QFeatures
#' @param initial_steps prefilled workflow steps
#'
#' @return return the server function for the processQFeatures app.
#' @rdname INTERNAL_build_process_server
#' @keywords internal
#'
#' @importFrom QFeatures QFeatures
#' @importFrom shiny observeEvent
#' @importFrom shinydashboard updateTabItems
#' @importFrom shinyalert shinyalert
#'
build_process_server <- function(qfeatures, initial_sets, initial_steps) {
    server <- function(input, output, session) {
        global_rv$exception_data <- data.frame(
            id = character(),
            title = character(),
            type = character(),
            func_call = character(),
            message = character(),
            full_message = character(),
            time = as.POSIXct(character()),
            stringsAsFactors = FALSE
        )
        .qf$qfeatures <- format_qfeatures(qfeatures, initial_sets)
        global_rv$workflow_config <- initial_steps
        global_rv$code_lines <- list()
        server_exception_menu(input, output, session)
        server_sidebar(input, output, session)
        server_module_workflow_config("workflow_config")
        server_dynamic_workflow(input, output, session)
        server_module_summary_tab("summary_tab")

        n_sets <- length(initial_sets)
        n_steps <- length(initial_steps)
        shinyalert(
            title = "App ready",
            text = paste0(
                "Loaded QFeatures with ", n_sets,
                " initial set", if (n_sets != 1) "s" else "", ".",
                if (n_steps > 0) {
                    paste0(
                        "\nWorkflow pre-configured with ", n_steps,
                        " step", if (n_steps != 1) "s" else "", "."
                    )
                } else {
                    ""
                }
            ),
            closeOnClickOutside = TRUE,
            type = "success",
            confirmButtonCol = "#3c8dbc"
        )
    }

    server
}

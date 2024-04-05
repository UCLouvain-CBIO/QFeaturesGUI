#' Server Builder
#'
#' @param sample_table a dataframe that contains
#'  the sample table given by the user
#' @param input_table a dataframe that contains
#'  the input table given by the user
#'
#' @return return the server function for the scpGUI app.
#' @rdname INTERNAL_build_server
#' @keywords internal
#'
#' @importFrom QFeatures QFeatures
#' @importFrom shiny observeEvent invalidateLater
#' @importFrom shinydashboard updateTabItems
#'
build_server <- function(sample_table, input_table) {
    server <- function(input, output, session) {
        global_rv$exception_data <- data.frame(
            title = character(),
            type = character(),
            func_call = character(),
            message = character(),
            full_message = character(),
            time = as.POSIXct(character())
        )
        global_rv$qfeatures <- QFeatures()
        global_rv$workflow_config <- list()
        server_exception_menu(input, output, session)
        server_sidebar(input, output, session)
        server_import_tab(
            input,
            output,
            session,
            sample_table,
            input_table
        )
        server_module_workflow_config("workflow_config")
        server_dynamic_workflow(input, output, session)
        # Force the selection of the import_tab at start
        session$onFlushed(function() {
            updateTabItems(session, "sidebar_menu", selected = "import_tab")
        })
        # Force the selection of the workflow_config_tab when the tabs are updated
        observeEvent(global_rv$workflow_config, {
            updateTabItems(session, "sidebar_menu", selected = "import_tab")
            updateTabItems(session, "sidebar_menu", selected = "workflow_config_tab")
        })

        server_module_summary_tab("summary_tab")
    }

    server
}

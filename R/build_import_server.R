#' Server Builder of Import app
#'
#' @param sample_table a dataframe that contains
#'  the sample table given by the user
#' @param input_table a dataframe that contains
#'  the input table given by the user
#'
#' @return return the server function for the importQFeatures app.
#' @rdname INTERNAL_build_import_server
#' @keywords internal
#'
#' @importFrom shiny observeEvent invalidateLater
#' @importFrom shinydashboard updateTabItems
#'
build_import_server <- function(sample_table, input_table) {
    server <- function(input, output, session) {
        global_rv$exception_data <- data.frame(
            title = character(),
            type = character(),
            func_call = character(),
            message = character(),
            full_message = character(),
            time = as.POSIXct(character())
        )
        server_exception_menu(input, output, session)
        server_import_tab(
            input,
            output,
            session,
            sample_table,
            input_table
        )
    }
    server
}

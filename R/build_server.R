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
        server_exception_menu(input, output, session)
        server_import_tab(
            input,
            output,
            session,
            sample_table,
            input_table
        )
        server_module_features_filtering_tab("PSM1")
        server_module_samples_filtering_tab("PSM2")
    }

    server
}

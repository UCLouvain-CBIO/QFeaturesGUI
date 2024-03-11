#' Server Builder
#'
#' @return return the server function for the scpGUI app.
#' @rdname INTERNAL_build_server
#' @keywords internal
#'
#' @importFrom QFeatures QFeatures
#'
build_server <- function() {
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
        server_import_tab(input, output, session)
        server_features_filtering_tab(input, output, session)
    }

    server
}

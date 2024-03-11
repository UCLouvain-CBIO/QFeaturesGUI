#' import tab (section) server builder
#'
#' @param input input parameter that should be given
#'  by the higher level server builder
#' @param output output parameter that should be given
#'  by the higher level server builder
#' @param session session parameter that should be given
#'  by the higher level server builder
#'
#' @return A shiny server function that contains the import tab server logic
#' @rdname INTERNAL_server_import_tab
#' @keywords internal
#'
server_import_tab <- function(input, output, session) {
    input_table <- box_read_table_server("input")
    sample_table <- box_read_table_server("sample")
    box_readscp_server(
        "readscp",
        input_table,
        sample_table
    )
}

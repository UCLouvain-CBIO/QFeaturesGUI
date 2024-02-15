server_import_page <- function(input, output, session, exception_data) {
    input_table <- box_read_table_server("input", exception_data)
    sample_table <- box_read_table_server("sample", exception_data)
    qfeatures <- box_readscp_server(
        "readscp",
        input_table,
        sample_table
    )
}

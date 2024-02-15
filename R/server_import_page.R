server_import_page <- function(input, output, session) {
    input_table <- box_read_table_server("input")
    sample_table <- box_read_table_server("sample")
    qfeatures <- box_readscp_server(
        "readscp",
        input_table,
        sample_table
    )
    observe(print(qfeatures()))
}

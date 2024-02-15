import_page <- function() {
    fluidRow(
        column(
            width = 6,
            box_read_table_ui("input"),
            box_read_table_ui("sample")
        ),
        column(
            width = 6,
            box_readscp_ui("readscp")
        )
    )
}

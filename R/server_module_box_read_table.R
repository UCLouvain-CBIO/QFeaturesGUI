box_read_table_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        table <- eventReactive(
            input$import_button,
            {
                req(input$file)
                loading("Be aware that this operation can be quite time consuming for large data sets")
                table <- error_handler(
                    read.table,
                    component_name = paste0("read.table ", id),
                    input$file$datapath,
                    sep = input$sep,
                    dec = input$dec,
                    skip = input$skip,
                    stringsAsFactors = input$stringsAsFactors,
                    comment.char = input$comment_char,
                    header = TRUE,
                    row.names = 1
                )
                removeModal()
                table
            }
        )


        output$dt_table <- renderDataTable({
            req(input$file)
            DT::datatable(table(),
                extensions = "FixedColumns",
                options = list(
                    searching = FALSE,
                    scrollX = TRUE,
                    fixedColumns = TRUE,
                    pageLength = 5,
                    lengthMenu = c(5, 10, 15)
                )
            )
        })

        table
    })
}

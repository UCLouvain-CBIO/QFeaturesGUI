box_read_table_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        table <- eventReactive(
            input$import_button,
            {
                req(input$file)
                read.table(input$file$datapath,
                    sep = input$sep,
                    dec = input$dec,
                    skip = input$skip,
                    stringsAsFactors = input$stringsAsFactors,
                    comment.char = input$comment_char,
                    header = TRUE,
                    row.names = 1
                )
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

        return(table)
    })
}

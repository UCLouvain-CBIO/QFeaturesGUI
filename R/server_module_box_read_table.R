#' A server module that contains the server logic to the read_table box module
#'
#' @param id module id
#'
#' @return the table reactiveVal that contains a dataframe
#' @rdname INTERNAL_box_read_table_server
#' @keywords internal
#'
#' @importFrom shiny moduleServer observe observeEvent reactiveVal req
#' @importFrom utils data read.table
box_read_table_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        table <- reactiveVal()
        observe(
            if (global_rv$example) {
                if (id == "sample") {
                    data("sampleAnnotation", package = "scp")
                    table(sampleAnnotation)
                }
                if (id == "input") {
                    data("mqScpData", package = "scp")
                    table(mqScpData)
                }
            }
        )

        observeEvent(
            input$import_button,
            {
                req(input$file)
                loading("Be aware that this operation can be quite time consuming for large data sets")
                new_table <- error_handler(
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
                table(new_table)
            }
        )


        output$dt_table <- renderDataTable({
            req(table)
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

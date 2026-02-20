#' A server module that contains the server logic to the read_table box module
#'
#' @param id module id
#' @param given_table a dataframe that contains the table given by the user
#'
#' @return the table reactiveVal that contains a dataframe
#' @rdname INTERNAL_box_read_table_server
#' @keywords internal
#'
#' @importFrom shiny moduleServer observe observeEvent reactiveVal req
#' @importFrom utils data read.table
box_read_table_server <- function(id, given_table = NULL) {
    moduleServer(id, function(input, output, session) {
        table <- reactiveVal()
        observe(
            if (!is.null(given_table)) {
                table(given_table)
            }
        )
        observeEvent(
            input$file,{
                shinyjs::enable("import_button")
            })
        observeEvent(
            input$import_button,
            {
                shinycssloaders::showPageSpinner(
                  type = "6",
                  caption = "Be aware that this operation can be quite time consuming for large datasets"
                  )
                req(input$file)
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
                shinycssloaders::hidePageSpinner()
                table(new_table)
                global_rv$code_lines[paste0("read_", id, "_data")] <- code_generator_read_table(
                    id = id,
                    file = paste0(id, "_table"),
                    sep = input$sep,
                    dec = input$dec,
                    skip = input$skip,
                    stringAsFactors = input$stringsAsFactors,
                    comment = input$comment_char
                )
                
            }
        )


        output$dt_table <- DT::renderDataTable({
            req(table())
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

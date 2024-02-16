box_readscp_server <- function(id, input_table, sample_table) {
    stopifnot(is.reactive(input_table))
    stopifnot(is.reactive(sample_table))

    moduleServer(id, function(input, output, session) {
        qfeatures <- eventReactive(input$convert, {
            error_handler(
                scp::readSCP,
                component_name = "QFeatures converting (readSCP)",
                featureData = input_table(),
                colData = sample_table(),
                batchCol = input$batch_col,
                channelCol = input$channel_col,
                removeEmptyCols = input$removeEmptyCols
            )
        })

        observe({
            updateSelectInput(session,
                "batch_col",
                choices = colnames(sample_table())
            )
            updateSelectInput(session,
                "channel_col",
                choices = colnames(sample_table())
            )
        })

        qfeatures_df <- reactive({
            req(qfeatures())
            error_handler(
                qfeatures_to_df,
                component_name = "qfeatures_to_df",
                qfeatures()
            )
        })

        output$qfeatures_dt <- renderDataTable({
            DT::datatable(qfeatures_df(),
                extensions = "FixedColumns",
                selection = "single",
                options = list(
                    searching = FALSE,
                    scrollX = TRUE,
                    fixedColumns = TRUE,
                    pageLength = 5,
                    lengthMenu = c(5, 10, 15)
                )
            )
        })

        output$assay_table <- DT::renderDataTable({
            if (!is.null(input$qfeatures_dt_rows_selected)) {
                row <- input$qfeatures_dt_rows_selected
                DT::datatable(
                    data.frame(SummarizedExperiment::assay(qfeatures()[[row]])),
                    extensions = "FixedColumns",
                    options = list(
                        searching = FALSE,
                        scrollX = TRUE,
                        fixedColumns = TRUE,
                        pageLength = 5,
                        lengthMenu = c(5, 10, 15, 20)
                    )
                )
            }
        })
        qfeatures
    })
}

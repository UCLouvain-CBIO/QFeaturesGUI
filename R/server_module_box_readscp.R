box_readscp_server <- function(id, input_table, sample_table) {
    stopifnot(is.reactive(input_table))
    stopifnot(is.reactive(sample_table))

    moduleServer(id, function(input, output, session) {
        qfeatures <- eventReactive(input$convert, {
            scp::readSCP(
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
        qfeatures
    })
}

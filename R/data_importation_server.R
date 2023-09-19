
data_importation_server<- function(input, output, session){

  # import and visualization of the input table
  observeEvent(input$import_input,{
    input_df <- file_to_df(input$input_table,
                            sep = input$input_sep,
                            dec = input$input_dec,
                            skip = input$input_skip,
                            stringsAsFactors = input$input_factor,
                            comment.char = input$input_com
                           )
    output$input_df_table <- DT::renderDataTable(
      {
        req(input$input_table)
        DT::datatable(input_df, extensions = 'FixedColumns',
                      options = list(
                        searching = FALSE,
                        scrollX = TRUE,
                        fixedColumns = TRUE)
        )
      }
    )
  })
  input_df <- eventReactive(input$import_input,
                            {
                              input_df <- file_to_df(input$input_table,
                                                     sep = input$input_sep,
                                                     dec = input$input_dec,
                                                     skip = input$input_skip,
                                                     stringsAsFactors = input$input_factor,
                                                     comment.char = input$input_com
                              )
                            })

  # import and visualization of the sample table
  observeEvent(input$import_sample,{
    sample_df <- file_to_df(input$sample_table,
                            sep = input$sample_sep,
                            dec = input$sample_dec,
                            skip = input$sample_skip,
                            stringsAsFactors = input$sample_factor,
                            comment.char = input$sample_com
    )
    output$sample_df_table <- DT::renderDataTable(
      {
        req(input$sample_table)
        DT::datatable(sample_df, extensions = 'FixedColumns',
                      options = list(
                        searching = FALSE,
                        scrollX = TRUE,
                        fixedColumns = TRUE)
        )
      }
    )

    colnames_sample <-{
      req(input$sample_table)
      updateSelectInput(session, "batch_col", choices = colnames(sample_df))
      updateSelectInput(session, "channel_col", choices = colnames(sample_df))
    }
  })

  sample_df <- eventReactive(input$import_sample,
                            {
                              sample_df <- file_to_df(input$sample_table,
                                                     sep = input$sample_sep,
                                                     dec = input$sample_dec,
                                                     skip = input$sample_skip,
                                                     stringsAsFactors = input$sample_factor,
                                                     comment.char = input$sample_com
                              )
                            })

  # Creation of the QFeatures object (readSCP_wraper is in utils.R)
  qfeat_converted <- reactive(
    readSCP_wraper(sample_table = input$sample_table,
                   input_table = input$input_table,
                   featureData = input_df(),
                   colData = sample_df(),
                   batch_col = input$batch_col,
                   channel_col = input$channel_col,
                   # suffix = input$suffix,
                   sep = input$sep,
                   removeEmptyCols = input$removeEmptyCols,
                   session = session
    )
  )

  observeEvent(input$convert, {
    showModal(modalDialog(
      title = "Loading",
      div(class = "progress",
          div(class = "progress-bar progress-bar-striped active", role = "progressbar",
              style = "width: 100%;")
      )
    ))
    qfeat_converted()  # Trigger the reactive expression to compute
    removeModal()
  })

  output$download_qfeat <- downloadHandler(
    filename = function() {
      "scp_qfeature_object.rds"
    },
    content = function(file) {
      saveRDS(qfeat_converted(), file)
    }
  )

}

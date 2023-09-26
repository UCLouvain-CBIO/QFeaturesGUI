data_importation_server<- function(input, output, session){

  # Import settings for the input table

  observeEvent(input$input_settings,{
    showModal(modalDialog(
      title = "Importation settings",
      textInput("input_sep", label = "Insert the separator character used", value = ","),
      textInput("input_dec", label = "Insert the decimal character used", value = "."),
      textInput("input_com", label = "Insert the comment character used", value = "#"),
      numericInput("input_skip", label = "Insert the number of line to skip before beginning to read data ", value = 0),
      checkboxInput("input_factor", label = "String as Factor", value = FALSE),
      actionButton("import_input", "Import", width = "100%",
                   style= "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      size = "s",
      easyClose = TRUE,
      footer = modalButton("Close", icon = icon("glyphicon glyphicon-remove", lib ='glyphicon'))
    )
    )
  })

  # Import settings for the sample table

  observeEvent(input$sample_settings,{
    showModal(modalDialog(
      title = "Importation settings",
      textInput("sample_sep", label = "Insert the separator character used", value = ","),
      textInput("sample_dec", label = "Insert the decimal character used", value = "."),
      textInput("sample_com", label = "Insert the comment character used", value = "#"),
      numericInput("sample_skip", label = "Insert the number of line to skip before beginning to read data ", value = 0),
      checkboxInput("sample_factor", label = "String as Factor", value = FALSE),
      actionButton("import_sample", "Import", width = "100%",
                   style= "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      size = "s",
      easyClose = TRUE,
      footer = modalButton("Close", icon = icon("glyphicon glyphicon-remove", lib ='glyphicon'))
      )
    )
  })

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
                        fixedColumns = TRUE,
                        pageLength = 5,
                        lengthMenu = c(5, 10, 15, 20))
        )
      }
    )
    removeModal()
  })

  # creation of a persistent object for the input dataframe
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
                        fixedColumns = TRUE,
                        pageLength = 5,
                        lengthMenu = c(5, 10, 15, 20))
        )
      }
    )

    colnames_sample <-{
      req(input$sample_table)
      updateSelectInput(session, "batch_col", choices = colnames(sample_df))
      updateSelectInput(session, "channel_col", choices = colnames(sample_df))
    }
    removeModal()
  })
  # creation of a persistent object for the sample dataframe
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
                   removeEmptyCols = input$removeEmptyCols,
                   session = session
    )
  )

  observeEvent(input$convert, { # On click on the convert button
    loading()
    qfeat_converted()  # Trigger the reactive expression to compute
    removeModal()
    # Preview of the QFeatures object
    if (class(qfeat_converted()) == "QFeatures"){
      qfeat_df <- qfeat_to_df(qfeat_converted())
      shinyjs::show("qfeat_preview")
      output$qfeat_table <- DT::renderDataTable(
        {
          DT::datatable(qfeat_df, extensions = 'FixedColumns',
                        selection = "single",
                        options = list(
                          searching = FALSE,
                          scrollX = TRUE,
                          fixedColumns = TRUE)
          )
        }
      )
      shinyjs::show("selection")
      # View of the assay of the selected row
      output$assay_table <- DT::renderDataTable(
        {
          if (!is.null(input$qfeat_table_rows_selected)){
            row <- input$qfeat_table_rows_selected
            DT::datatable(data.frame(SummarizedExperiment::assay(qfeat_converted()[[row]])), 
                        extensions = 'FixedColumns',
                        options = list(
                          searching = FALSE,
                          scrollX = TRUE,
                          fixedColumns = TRUE,
                          pageLength = 5,
                          lengthMenu = c(5, 10, 15, 20))
          )
          }
        }
      )
    }
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

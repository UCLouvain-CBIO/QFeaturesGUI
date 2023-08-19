library(shiny)

# define module to render a csv into a DT object

data_importation_server<- function(input, output, session){

  # import and visualization of the input table
  input_df <- reactive(file_to_df(input$input_table))
  output$input_df_table <- DT::renderDataTable(
    {
      req(input$input_table)
      DT::datatable(input_df(), extensions = 'FixedColumns',
                    options = list(
                      searching = FALSE,
                      scrollX = TRUE,
                      fixedColumns = TRUE)
      )
    }
  )

  # import and visualization of the sample table
  sample_df <- reactive(file_to_df(input$sample_table))
  output$sample_df_table <- DT::renderDataTable(
    {
      req(input$sample_table)
      DT::datatable(sample_df(), extensions = 'FixedColumns',
                    options = list(
                      searching = FALSE,
                      scrollX = TRUE,
                      fixedColumns = TRUE)
      )
    }
  )

  # Get the columns names of the sample table
  colnames_sample <- reactive({
    req(input$sample_table)
    colnames(sample_df())
  })

  output$batch_col_sel <- renderUI({
    selectInput("batch_col", "Choose the approriate batch column :", colnames_sample() )
  })

  output$channel_col_sel <- renderUI({
    selectInput("channel_col", "Choose the approriate channel column :", colnames_sample() )
  })

  # Creation of the QFeatures object (readSCP_wraper is in utils.R)
  qfeat_converted <- reactive(
    readSCP_wraper(sample_table = input$sample_table,
                   input_table = input$input_table,
                   featureData = input_df(),
                   colData = sample_df(),
                   batch_col = input$batch_col,
                   channel_col = input$channel_col
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

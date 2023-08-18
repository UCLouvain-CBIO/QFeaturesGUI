# If the app becomes complex, we could use a framework like shinydashboard
#' @title scpGUI
#'
#' @description scpGui is a simple graphical interface to handle SCP data.
#' The first page of the app allow to convert two tables (Input and Sample) to a QFeatures object.
#' This process is performed with the \code{\link[scp]{readSCP}} function from the scp package.
#'
#' @return Launch the scpGui shiny app.
#' @export
#' @import shiny
#' @import scp
#' @importFrom DT renderDataTable datatable
#' @examples
#' scpGui() #start a shiny app
#'
scpGUI <- function(){
  if(interactive()){

    ui <- shinyUI(
      navbarPage("scp GUI",
                 data_importation_tab,
                 tabPanel("Quality Control", "Under developement ...")
      ))

    server <- function(input, output, session) {

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

      readSCP_wraper <- function(sample_table,
                                 input_table,
                                 featureData,
                                 colData,
                                 batch_col,
                                 channel_col){
        tryCatch(

          {
            req(sample_table, input_table)
            readSCP(
              featureData = featureData,
              colData = colData,
              batchCol = batch_col,
              channelCol = channel_col
            )
            showNotification("Convertion succed !")
          },
          #if an error occurs, tell me the error
          error = function(err) {
            # Display the error message in the container
            showNotification(paste0("An error occured : ", err),duration = 10)
            },
          #if a warning occurs, tell me the warning
          warning=function(war) {
            showNotification(paste0("A warning occured : ", war), duration = 10)
          }
        )
      }

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

    shinyApp(ui = ui, server = server)
  }
}


# todo :
# - Delimiter parameter for the importation
# - Improve the error view
# - Make the converter menu always visible
# - More parameter for the readSCP()
# - More descriptive pop up
# - Documentation
# - Package format
# - Refactoring (see CytoPipelineGui)
# - Insert scp package ref to the doc

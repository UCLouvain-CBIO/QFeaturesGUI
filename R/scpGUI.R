# If the app becomes complex, we could use a framework like shinydashboard
#' @title A simple graphical interface to import SCP data
#'
#' @description scpGUI is a simple graphical interface to handle SCP data.
#' The first page of the app allow to convert two tables (Input and Sample) to a QFeatures object.
#' This process is performed with the \code{\link[scp]{readSCP}} function from the scp package.
#'
#' @return Launch the scpGUIImport shiny app.
#' @export
#' @import shiny
#' @importFrom SummarizedExperiment assay
#' @importFrom scp readSCP
#' @importFrom shinyBS createAlert bsAlert
#' @importFrom DT renderDataTable datatable
#' @importFrom shinyjs useShinyjs hidden showElement
#' @examples
#' scpGUIImport() #start a shiny app
#'
scpGUIImport <- function(){
  if(interactive()){
    ui <- tagList(
      shinyjs::useShinyjs(),
      navbarPage("scpGUIImport",
                 data_importation_tab,
                 tabPanel("Quality Control", "Under developement ...")
      ))

    server <- function(input, output, session) {
      options(shiny.maxRequestSize=1000*1024^2)
      data_importation_server(input, output, session)
    }

    shinyApp(ui = ui, server = server)
  }
}

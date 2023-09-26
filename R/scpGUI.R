# If the app becomes complex, we could use a framework like shinydashboard
#' @title A simple graphical interface to handle SCP data
#'
#' @description scpGui is a simple graphical interface to handle SCP data.
#' The first page of the app allow to convert two tables (Input and Sample) to a QFeatures object.
#' This process is performed with the \code{\link[scp]{readSCP}} function from the scp package.
#'
#' @return Launch the scpGui shiny app.
#' @export
#' @import shiny
#' @import scp
#' @import QFeatures
#' @import shinyBS
#' @import shinyjs
#' @importFrom DT renderDataTable datatable
#' @examples
#' scpGUI() #start a shiny app
#'
scpGUI <- function(){
  if(interactive()){
    ui <- shinyUI(
      navbarPage("scpGUI",
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


# todo :
# - modify error message for read.table

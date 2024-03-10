#' Title
#'
#' @param input 
#' @param output 
#' @param session 
#'
#' @return
#' 
#' @importFrom shiny moduleServer renderPlot
#' @importFrom nipals nipals
#'
#' @examples
server_module_qc_metrics <- function(id, input, output, session) {
    moduleServer(id, function(input, output, session){
        observe({
            req(global_rv$qfeatures_object)
            print("test")
            updateSelectInput(session,
                "selected_assay",
                choices = names(global_rv$qfeatures_object)
            )
        })
    })
}

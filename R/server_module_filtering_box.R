#' @title server_module_annotation_plot
#'
#' @param id The module id
#' @param qfeat A reactiveVal that contains the different assays that will be used in the module
#' @param type A character string specifying the type of the filtering (sample or feature based)
#' @return The updated assays
#' @rdname INTERNAL_server_module_filtering_box
#' @keywords internal
#' 
#' @importFrom shiny moduleServer updateSelectInput reactive observe is.reactive
#' @importFrom SummarizedExperiment colData rowData
#' 
server_module_filtering_box <- function(id, qfeat, type) {
    is.reactive(qfeat)
    moduleServer(id, function(input, output, session) {
        annotations <- reactive({
            req(qfeat())
            if (type == "samples") {
                colnames(colData(qfeat()[[1]]))
            } else if (type == "features") {
                colnames(rowData(qfeat()[[1]]))
            }
        })
        observe({
            updateSelectInput(
                inputId = "annotation_selection",
                choices = as.character(annotations())
            )
        })
        server_module_annotation_plot(
            "annotation_plot",
            qfeat,
            type,
            input$filter_value,
            input$annotation_selection
        )
    })
}

#' @title Annotation Plot server module
#' 
#' @param id The module id
#' @param qfeat A reactiveVal that contains the different assays that will be used in the module
#' @param type A character string specifying the type of the filtering (sample or feature based)
#' @param filter_value The value that will be used in combinaison with the filter operator
#' @param annotation The annotation that will be used to filter the data
#' @return A plot of the distribution of the selected annotation in a specific assay
#' @rdname INTERNAL_server_module_annotation_plot
#' @keywords internal
#'
#' @importFrom shiny moduleServer observe req eventReactive reactive
#'
server_module_annotation_plot <- function(
        id,
        qfeat,
        type,
        filter_value,
        annotation) {
    moduleServer(id, function(input, output, session) {
        observe({
            req(qfeat())
            updateSelectInput(
                inputId = "selected_assay",
                choices = names(qfeat())
            )
        })
        observe({
            req(input$selected_assay)
            print(input$selected_assay)})
        output$plot <- renderPlotly({
            print(input$selected_assay)
            plot_ly( x = 1:10, y = 1:10, type = 'scatter', mode = 'markers')
        })    
})}

#' Server for the module normalisation tab
#'
#' @param id module id
#' @return The server logic for the normalisation tab
#' @rdname INTERNAL_server_module_normalisation_tab
#' @keywords internal
#'
#' @importFrom shiny moduleServer updateSelectInput observeEvent eventReactive is.reactive
#' @importFrom MultiAssayExperiment getWithColData
#'
server_module_normalisation_tab <- function(id, step_number) {
    moduleServer(id, function(input, output, session) {
        assays_to_process <- eventReactive(input$reload, {
            error_handler(page_assays_subset,
                component_name = "Page assays subset",
                qfeatures = global_rv$qfeatures,
                pattern = paste0("_(QFeaturesGUI#", step_number - 1, ")")
            )
        })

        processed_assays <- reactive({
            req(assays_to_process())
            error_handler(
                normalisation_qfeatures,
                component_name = "Normalisation",
                qfeatures = assays_to_process(),
                method = input$method
            )
        })

        output$density_plot <- renderPlotly({
            req(processed_assays())
            density_by_sample_plotly(
                qfeatures = processed_assays(),
                color = input$color
            )
            # error_handler(
            #     density_by_sample_plotly,
            #     component_name = "Density by sample plotly",
            #     qfeatures = processed_assays(),
            #     color = input$color)
        })

        observe({
            req(processed_assays())
            updateSelectInput(session,
                "color",
                choices = colnames(colData(processed_assays()))
            )
        })

        observeEvent(input$export, {
            req(processed_assays())
            loading(paste("Be aware that this operation",
                "can be quite time consuming for large data sets",
                sep = " "
            ))
            error_handler(
                add_assays_to_global_rv,
                component_name = "Add assays to global_rv",
                processed_qfeatures = processed_assays(),
                step_number = step_number,
                type = "normalisation"
            )
            removeModal()
        })
    
    })
}
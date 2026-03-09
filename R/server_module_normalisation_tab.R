#' Server for the module normalisation tab
#'
#' @param id module id
#' @return The server logic for the normalisation tab
#' @rdname INTERNAL_server_module_normalisation_tab
#' @keywords internal
#'
#' @importFrom shiny moduleServer updateSelectInput observeEvent reactive is.reactive
#' @importFrom MultiAssayExperiment getWithColData
#'
server_module_normalisation_tab <- function(id, step_number, step_rv, parent_rv) {
    moduleServer(id, function(input, output, session) {
        pattern <- paste0("_(QFeaturesGUI#", step_number - 1, ")")

        step_ready <- reactive({
            if (!is.null(parent_rv)) req(parent_rv() > 0L)
            TRUE
        })

        parent_assays <- reactive({
            req(step_ready())
            error_handler(page_assays_subset,
                component_name = "Page assays subset",
                qfeatures = .qf$qfeatures,
                pattern = pattern
            )
        })

        processed_assays <- reactive({
            req(parent_assays())
            error_handler(
                normalisation_qfeatures,
                component_name = "Normalisation",
                qfeatures = parent_assays(),
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
            step_rv(step_rv() + 1L)
            removeModal()
        })
    })
}

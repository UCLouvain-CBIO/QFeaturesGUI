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

        clicked <- reactiveVal(FALSE)
        observeEvent(input$apply_normalisation, {
            clicked(TRUE)
        })

        output$post_density_message <- renderText({
            if (!clicked()) {
                "The post-normalisation plot will be displayed once you apply normalisation."
            } else {
                ""
            }
        })

        processed_assays <- eventReactive(input$apply_normalisation, {
            req(parent_assays())
            with_task_loader(
                caption = "Applying normalisation and generating post-normalisation densities",
                expr = {
                    error_handler(
                        normalisation_qfeatures,
                        component_name = "Normalisation",
                        qfeatures = parent_assays(),
                        method = input$method
                    )
                }
            )
        })

        selected_color <- reactive({
            req(input$color)
            if (identical(input$color, "NULL")) {
                return(NULL)
            }
            input$color
        })

        output$density_plot_pre <- renderPlotly({
            req(parent_assays())
            error_handler(
                density_by_sample_plotly,
                component_name = "Pre-normalisation density plot",
                qfeatures = parent_assays(),
                color = selected_color(),
                title = "Pre-normalisation density"
            )
        })

        output$density_plot_post <- renderPlotly({
            req(processed_assays())
            error_handler(
                density_by_sample_plotly,
                component_name = "Post-normalisation density plot",
                qfeatures = processed_assays(),
                color = selected_color(),
                title = "Post-normalisation density"
            )
        })

        observe({
            req(parent_assays())
            choices <- c("NULL", colnames(colData(parent_assays())))
            selected <- "NULL"
            if (!is.null(input$color) && input$color %in% choices) {
                selected <- input$color
            }
            updateSelectInput(session,
                "color",
                choices = choices,
                selected = selected
            )
        })

        observeEvent(input$export,
            {
                req(processed_assays())
                with_task_loader(
                    caption = paste(
                        "Be aware that this operation",
                        "can be quite time consuming for large data sets"
                    ),
                    expr = {
                        error_handler(
                            add_assays_to_global_rv,
                            component_name = "Add assays to global_rv",
                            processed_qfeatures = processed_assays(),
                            step_number = step_number,
                            type = "normalisation"
                        )
                        step_rv(step_rv() + 1L)
                        global_rv$codeLines[[paste0("normalisation_", step_number)]] <- codeGeneratorNormalisation(method = input$method)
                    }
                )
            },
            ignoreInit = TRUE
        )
    })
}

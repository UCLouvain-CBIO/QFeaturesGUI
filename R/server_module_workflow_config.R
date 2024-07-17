#'
#' @title Workflow Configuration Module Server
#'
#' @param id The module id
#'
#' @return A Shiny module UI function
#' @rdname INTERNAL_server_module_workflow_config
#' @keywords internal
#'
#' @importFrom shiny moduleServer observeEvent renderUI selectInput reactiveVal reactiveValues reactiveValuesToList reactive
#' @importFrom htmltools div
#'

server_module_workflow_config <- function(id) {
    moduleServer(id, function(input, output, session) {
        n_steps <- reactiveVal(0)
        steps <- reactiveValues()

        observeEvent(input$add, {
            n_steps(n_steps() + 1)
        })

        observeEvent(input$remove, {
            if (n_steps() > 0) {
                n_steps(n_steps() - 1)
            }
        })

        observeEvent(n_steps(), {
            output$tabs <- renderUI({
                if (n_steps() > 0) {
                    lapply(seq_len(n_steps()), function(i) {
                        selected <- "Samples Filtering"
                        if (!is.null(steps[[paste0("step_", i)]])) {
                            selected <- steps[[paste0("step_", i)]]
                        }
                        div(
                            selectInput(
                                inputId = NS(id, paste0("step_", i)),
                                label = paste0("Step ", i),
                                choices = c(
                                    "Samples Filtering",
                                    "Features Filtering",
                                    "Log Transformation"
                                ),
                                selected = selected,
                                width = "90%"
                            ),
                            class = "container widget-box",
                            style = "width: 80%;"
                        )
                    })
                }
            })
            if (n_steps() > 0) {
                lapply(seq_len(n_steps()), function(i) {
                    observe({
                        steps[[paste0("step_", i)]] <- input[[paste0("step_", i)]]
                    })
                })
            }
        })
        steps_list <- reactive({
            full_list <- reactiveValuesToList(steps)
            lapply(seq_len(n_steps()), function(i) {
                full_list[[i]]
            })
        })

        observeEvent(input$apply, {
            global_rv$workflow_config <- steps_list()
        })
    })
}

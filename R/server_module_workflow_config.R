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
                        selected <- "Cells Filtering"
                        if (!is.null(steps[[paste0("step_", i)]])) {
                            selected <- steps[[paste0("step_", i)]]
                        }
                        selectInput(
                            inputId = NS(id, paste0("step_", i)),
                            label = paste0("Step ", i),
                            choices = c(
                                "Cells Filtering",
                                "Features Filtering"
                            ),
                            selected = selected
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
            reactiveValuesToList(steps)
        })

        observeEvent(input$apply, {
            global_rv$workflow_config <- steps_list()
        })
    })
}

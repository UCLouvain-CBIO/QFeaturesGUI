#' Server for the module log transformation tab
#'
#' @param id module id
#' @return The server logic for the log transformation tab
#' @rdname INTERNAL_server_module_log_transformation_tab
#' @keywords internal
#'
#' @importFrom shiny moduleServer updateSelectInput observeEvent eventReactive is.reactive
#' @importFrom MultiAssayExperiment getWithColData
#'
server_module_log_transform_tab <- function(id, step_number) {
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
                log_transform_qfeatures,
                component_name = "Log transformation",
                qfeatures = assays_to_process(),
                base = as.integer(input$log_base),
                pseudocount = as.integer(input$pseudocount)
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
                type = "log_transformation"
            )
            removeModal()
        })
    })
    }
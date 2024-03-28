#' @title Server logic for the features filtering tab
#' @param id The module id
#'
#' @return The processed assays
#' @rdname INTERNAL_server_module_features_filtering_tab
#' @keywords internal
#'
#' @importFrom shiny moduleServer eventReactive observeEvent renderUI reactiveValues observe reactiveValuesToList NS reactive
#' @importFrom QFeatures filterFeatures
#'
server_module_features_filtering_tab <- function(id) {
    moduleServer(id, function(input, output, session) {
        assays_to_process <- eventReactive(input$reload, {
            error_handler(page_assays_subset,
                component_name = "Page assays subset",
                qfeatures = global_rv$qfeatures,
                pattern = "_(scpGUI#1)"
            )
        })
        filtering_conditions <- reactiveValues()
        server_module_pre_qc_metrics("psm_pre", assays_to_process)
        observeEvent(input$generate_boxes, {
            output$filtering_boxes <- renderUI({
                if (input$n_boxes > 0) {
                    lapply(1:input$n_boxes, function(i) {
                        # Call the server part of the filtering box module and store the output
                        interface_module_filtering_box(
                            NS(id, paste0("filtering_", i))
                        )
                    })
                }
            })
            if (input$n_boxes > 0) {
                lapply(1:input$n_boxes, function(i) {
                    condition <- server_module_filtering_box(
                        paste0("filtering_", i),
                        assays_to_process,
                        "features"
                    )


                    # Use an observe function to update the reactiveValues object
                    observe({
                        filtering_conditions[[paste0("condition_", i)]] <- condition()
                    })
                })
            }
        })
        filtering_conditions_list <- reactive({
            reactiveValuesToList(filtering_conditions)
        })
        entire_condition <- reactive({
            res <- lapply(filtering_conditions_list(), function(condition) {
                condition
            })
            res <- unlist(res)
            if (length(filtering_conditions_list()) > 0) {
                return(as.formula(paste0("~", paste(res, collapse = " & "))))
            } else {
                return(NULL)
            }
        })
        processed_assays <- reactive({
            if (length(filtering_conditions_list() > 0)) {
                return(error_handler(filterFeatures,
                    component_name = "Filter features",
                    object = assays_to_process(),
                    filter = entire_condition()
                ))
            } else {
                return(assays_to_process())
            }
        })
        server_module_pre_qc_metrics("psm_filtered", processed_assays)
    })
}

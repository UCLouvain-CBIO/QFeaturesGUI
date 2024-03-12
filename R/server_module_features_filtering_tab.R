server_module_features_filtering_tab <- function(id) {
    moduleServer(id, function(input, output, session) {
        assays_to_process <- eventReactive(input$reload, {
            error_handler(page_assays_subset,
                component_name = "Page assays subset",
                qfeatures = global_rv$qfeatures,
                pattern = "_(scpGUI#1)"
            )
        })
        server_module_pre_qc_metrics("psm_pre", assays_to_process)
        server_module_pre_qc_metrics("psm_filtered", assays_to_process)
    })
}

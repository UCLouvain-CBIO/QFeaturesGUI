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
        
        processed_assays_1 <- server_module_filtering_box(
            "filtering_1",
            assays_to_process,
            "features"
        )
        processed_assays_2 <- server_module_filtering_box(
            "filtering_2",
            processed_assays_1,
            "features"
        )
        processed_assays_3 <- server_module_filtering_box(
            "filtering_3",
            processed_assays_2,
            "features"
        )
        processed_assays_4 <- server_module_filtering_box(
            "filtering_4",
            processed_assays_3,
            "features"
        )
        server_module_pre_qc_metrics("psm_filtered", processed_assays_4)
    })
}

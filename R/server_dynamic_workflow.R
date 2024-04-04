server_dynamic_workflow <- function(input, output, session) {
    observeEvent(global_rv$workflow_config, {
        output$all_tabs <- renderUI({
            static_tabs <- list(
                tabItem(
                    tabName = "import_tab",
                    import_tab()
                ),
                tabItem(
                    tabName = "workflow_config_tab",
                    interface_module_workflow_config_tab("workflow_config")
                ),
                tabItem(
                    tabName = "features_filtering_tab",
                    interface_module_features_filtering_tab("PSM1")
                ),
                tabItem(
                    tabName = "cell_filtering_tab",
                    interface_module_samples_filtering_tab("PSM2")
                ),
                tabItem(
                    tabName = "psm_na_report_tab",
                    "WIP"
                ),
                tabItem(
                    tabName = "peptides_na_report_tab",
                    "WIP"
                )
            )

            dynamic_tabs <- lapply(seq_along(global_rv$workflow_config), function(i) {
                tabItem(
                    tabName = paste0("step_", i),
                    fluidRow(
                        column(
                            width = 12,
                            h4("Step Description"),
                            p("This is a description of the step.")
                        )
                    )
                )
            })

            do.call(tabItems, c(static_tabs, dynamic_tabs))
        })
    })
}

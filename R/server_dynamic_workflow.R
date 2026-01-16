#' Server logic for the dynamic workflow
#'
#' @param input input from the parent server function
#' @param output output from the parent server function
#' @param session session from the parent server function
#'
#' @return The server logic for the dynamic workflow
#' @rdname INTERNAL_server_dynamic_workflow
#' @keywords internal
#'
#' @importFrom shiny observeEvent renderUI
#' @importFrom shinydashboard tabItem tabItems
#' @importFrom htmltools h2
#'

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
                    tabName = "summary_tab",
                    interface_module_summary_tab("summary_tab")
                )
            )

            dynamic_tabs <- lapply(seq_along(global_rv$workflow_config), function(i) {
                tabItem(
                    tabName = paste0("step_", i),
                    if (global_rv$workflow_config[[i]] == "Samples Filtering") {
                        interface_module_samples_filtering_tab(
                            paste0("samples_filtering_", i)
                        )
                    } else if (global_rv$workflow_config[[i]] == "Features Filtering") {
                        interface_module_features_filtering_tab(
                            paste0("features_filtering_", i)
                        )
                    } else if (global_rv$workflow_config[[i]] == "Log Transformation") {
                        interface_module_log_transform_tab(
                            paste0("log_transform_", i)
                        )
                    } else if (global_rv$workflow_config[[i]] == "Normalisation") {
                        interface_module_normalisation_tab(
                            paste0("normalisation_", i)
                        )
                    }
                )
            })

            do.call(tabItems, c(static_tabs, dynamic_tabs))
        })

        lapply(seq_along(global_rv$workflow_config), function(i) {
            if (global_rv$workflow_config[[i]] == "Samples Filtering") {
                server_module_samples_filtering_tab(paste0("samples_filtering_", i),
                    step_number = i
                )
            } else if (global_rv$workflow_config[[i]] == "Features Filtering") {
                server_module_features_filtering_tab(paste0("features_filtering_", i),
                    step_number = i
                )
            } else if (global_rv$workflow_config[[i]] == "Log Transformation") {
                server_module_log_transform_tab(paste0("log_transform_", i),
                    step_number = i
                )
            } else if (global_rv$workflow_config[[i]] == "Normalisation") {
                server_module_normalisation_tab(paste0("normalisation_", i),
                    step_number = i
                )
            }
        })
    })
}


server_dynamic_import_workflow <- function(input, output, session) {
  observeEvent(global_rv$workflow_config, {
    output$all_tabs <- renderUI({
      static_tabs <- list(tabItem(
        tabName = "import_tab",
        import_tab()
      ))
      do.call(tabItems, c(static_tabs))
    })
  })
}


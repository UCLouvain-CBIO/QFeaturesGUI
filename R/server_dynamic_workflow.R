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
    observe({
        lapply(seq_along(global_rv$workflow_config), function(i) {
            output[[paste0("dynamic_step_ui_", i)]] <- renderUI({
                switch(global_rv$workflow_config[[i]],
                    "Sample Filtering" = interface_module_samples_filtering_tab(paste0("sample_filtering_", i)),
                    "Feature Filtering" = interface_module_features_filtering_tab(paste0("feature_filtering_", i)),
                    "Log Transformation" = interface_module_log_transform_tab(paste0("log_transform_", i)),
                    "Normalisation" = interface_module_normalisation_tab(paste0("normalisation_", i))
                )
            })

            # Call the corresponding server module
            switch(global_rv$workflow_config[[i]],
                "Sample Filtering" = server_module_samples_filtering_tab(paste0("sample_filtering_", i), step_number = i),
                "Feature Filtering" = server_module_features_filtering_tab(paste0("feature_filtering_", i), step_number = i),
                "Log Transformation" = server_module_log_transform_tab(paste0("log_transform_", i), step_number = i),
                "Normalisation" = server_module_normalisation_tab(paste0("normalisation_", i), step_number = i)
            )
        })
    })
}

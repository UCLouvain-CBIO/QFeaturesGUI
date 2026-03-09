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
#' @importFrom shiny observeEvent renderUI outputOptions
#' @importFrom shinydashboard tabItem tabItems
#' @importFrom htmltools h2
#'

server_dynamic_workflow <- function(input, output, session) {
    observe({
        step_rvs <- lapply(
            seq_along(global_rv$workflow_config),
            function(i) reactiveVal(0)
        )
        global_rv$step_rvs <- step_rvs

        lapply(seq_along(global_rv$workflow_config), function(i) {
            parent_rv <- if (i == 1) NULL else step_rvs[[i - 1]]

            output[[paste0("dynamic_step_ui_", i)]] <- renderUI({
                switch(global_rv$workflow_config[[i]],
                    "Sample Filtering" = interface_module_samples_filtering_tab(paste0("sample_filtering_", i)),
                    "Feature Filtering" = interface_module_features_filtering_tab(paste0("feature_filtering_", i)),
                    "Log Transformation" = interface_module_log_transform_tab(paste0("log_transform_", i)),
                    "Normalisation" = interface_module_normalisation_tab(paste0("normalisation_", i))
                )
            })
            # Disable suspend-when-hidden so the UI renders eagerly on the
            # first flush, ensuring selectInputs exist before updateSelectInput
            # messages arrive from step module observers.
            outputOptions(output, paste0("dynamic_step_ui_", i), suspendWhenHidden = FALSE)

            # Call the corresponding server module
            switch(global_rv$workflow_config[[i]],
                "Sample Filtering" = server_module_samples_filtering_tab(paste0("sample_filtering_", i), step_number = i, step_rv = step_rvs[[i]], parent_rv = parent_rv),
                "Feature Filtering" = server_module_features_filtering_tab(paste0("feature_filtering_", i), step_number = i, step_rv = step_rvs[[i]], parent_rv = parent_rv),
                "Log Transformation" = server_module_log_transform_tab(paste0("log_transform_", i), step_number = i, step_rv = step_rvs[[i]], parent_rv = parent_rv),
                "Normalisation" = server_module_normalisation_tab(paste0("normalisation_", i), step_number = i, step_rv = step_rvs[[i]], parent_rv = parent_rv)
            )
        })
    })
}

#' Server logic for the 'viz_box' module
#' @param id The module id
#' @param assays_to_process The assays to process (reactive)
#' @return The server logic for the 'viz_box' module
#'
#' @rdname INTERNAL_server_module_viz_box
#' @keywords internal
#'
#' @importFrom shiny moduleServer observe req reactive updateSelectizeInput updateSelectInput
#' @importFrom SummarizedExperiment assay colData
#' @importFrom plotly plot_ly renderPlotly add_trace layout
#'

server_module_viz_box <- function(id, assays_to_process) {
    moduleServer(id, function(input, output, session) {
        unique_features <- reactive({
            assays_to_process()
            features_list <- lapply(seq_along(assays_to_process()), function(i) {
                rownames(assay(assays_to_process()[[i]]))
            })
            features_vector <- unlist(features_list)
            unique(features_vector)
        })

        observe({
            req(unique_features())
            updateSelectizeInput(session,
                "feature",
                choices = unique_features(),
                server = TRUE
            )
        })

        observe({
            req(assays_to_process())
            updateSelectInput(session,
                "sample_type_column",
                choices = colnames(colData(assays_to_process()))
            )
        })

        feature_summary_df <- reactive({
            req(assays_to_process())
            req(input$feature)
            res_df <- data.frame(
                "sample_type" = character(),
                "intensity" = numeric()
            )
            for (i in names(assays_to_process())) {
                tryCatch(
                    {
                        sub_assay <- getWithColData(assays_to_process(), i)
                        sub_assay_data <- assay(sub_assay)
                        if (input$scale) {
                            sub_assay_data <- scale(sub_assay_data)
                        }
                        sub_assay_df <- data.frame(
                            "sample_type" = colData(sub_assay)[, input$sample_type_column],
                            "intensity" = sub_assay_data[input$feature, ]
                        )
                        res_df <- rbind(res_df, sub_assay_df)
                    },
                    error = function(e) {
                        NULL
                    }
                )
            }
            res_df
        })

        output$plot <- renderPlotly({
            req(feature_summary_df())
            plot_ly(feature_summary_df(),
                y = ~intensity,
                x = ~sample_type,
                type = "box",
                boxpoints = "all",
                color = ~sample_type
            ) %>%
                layout(
                    xaxis = list(
                        title = "Sample Type",
                        showticklabels = FALSE
                    ),
                    yaxis = list(title = "Intensity")
                )
        })
    })
}

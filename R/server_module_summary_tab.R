#' Summary tab module server
#'
#' @param id the id of the module
#' @return a server module for the summary tab
#'
#' @rdname INTERNAL_server_module_summary_tab
#' @keywords internal
#'
#' @importFrom DT renderDataTable datatable
#' @importFrom SummarizedExperiment assay
#' @importFrom shiny moduleServer observe reactive
#' @importFrom plotly renderPlotly plot_ly

server_module_summary_tab <- function(id) {
    moduleServer(id, function(input, output, session) {
        qfeatures_df <- reactive({
            error_handler(
                qfeatures_to_df,
                component_name = "qfeatures_to_df",
                global_rv$qfeatures
            )
        })

        output$qfeatures_dt <- DT::renderDataTable({
            DT::datatable(qfeatures_df(),
                extensions = "FixedColumns",
                selection = "single",
                options = list(
                    searching = FALSE,
                    scrollX = TRUE,
                    fixedColumns = TRUE,
                    pageLength = 5,
                    lengthMenu = c(5, 10, 15)
                )
            )
        })

        output$assay_table <- DT::renderDataTable({
            if (!is.null(input$qfeatures_dt_rows_selected)) {
                row <- input$qfeatures_dt_rows_selected
                DT::datatable(
                    data.frame(assay(global_rv$qfeatures[[row]])),
                    extensions = "FixedColumns",
                    options = list(
                        searching = FALSE,
                        scrollX = TRUE,
                        fixedColumns = TRUE,
                        pageLength = 5,
                        lengthMenu = c(5, 10, 15, 20)
                    )
                )
            }
        })

        output$qfeatures_plot <- renderPlotly({
            if (length(global_rv$qfeatures) > 0) {
                empty_qfeatures <- global_rv$qfeatures[1, ]
                names(empty_qfeatures) <- remove_QFeaturesGUI(names(empty_qfeatures))
                plot(empty_qfeatures,
                    interactive = TRUE
                )
            }
        })

        output$download_qfeatures <- downloadHandler(
            filename = function() {
                "qfeatures_object.rds"
            },
            content = function(file) {
                final_qfeatures <- global_rv$qfeatures
                names(final_qfeatures) <- remove_QFeaturesGUI(names(final_qfeatures))
                saveRDS(final_qfeatures, file)
            }
        )
    })
}

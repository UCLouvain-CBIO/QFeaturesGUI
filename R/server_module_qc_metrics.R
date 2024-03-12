#' Title
#'
#' @param id module id
#' @param assays_to_process a reactiveVal that contains the different assays that will be used in the module
#' @return The different assays that will be processed in the page
#' @rdname INTERNAL_server_module_qc_metrics
#' @keywords internal
#'
#' @importFrom shiny moduleServer updateSelectInput observeEvent eventReactive
#' @importFrom pcaMethods pca scores
#' @importFrom ggplot2 ggplot aes geom_point xlab ylab
#' @importFrom plotly ggplotly renderPlotly
#'
server_module_pre_qc_metrics <- function(id, assays_to_process) {
    moduleServer(id, function(input, output, session) {
        observe({
            updateSelectInput(session,
                "selected_assay",
                choices = names(assays_to_process())
            )
        })
        output$pca <- renderPlotly({
            if (is.null(assays_to_process())) {
                return()
            }
            req(input$selected_assay)
            req(assays_to_process)
            pca <- pcaMethods_wraper(
                assays_to_process()[[input$selected_assay]],
                method = "nipals"
            )
            df <- data.frame(scores(pca))
            # TODO: Add color (e.g. cell type), annotation.
            # TODO: Add a table with the selected points.
            # TODO: make the pca
            # + selected points reactive to the table. A module?
            plot <- ggplot(df, aes(x = PC1, y = PC2, text = rownames(df))) +
                geom_point() +
                xlab(paste(
                    "PC1", round(pca@R2[1] * 100, 2),
                    "% of the variance"
                )) +
                ylab(paste(
                    "PC2", round(pca@R2[2] * 100, 2),
                    "% of the variance"
                ))

            ggplotly(plot, tooltip = "text")
        })
    })
}

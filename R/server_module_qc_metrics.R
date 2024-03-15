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
#' @importFrom MultiAssayExperiment getWithColData
#'
server_module_pre_qc_metrics <- function(id, assays_to_process) {
    moduleServer(id, function(input, output, session) {
        observe({
            updateSelectInput(session,
                "selected_assay",
                choices = names(assays_to_process())
            )
        })
        single_assay <- reactive({
            req(input$selected_assay)
            # Warning appears here
            # Warning message: 'experiments' dropped; see 'drops()'
            # see with Chris
            getWithColData(assays_to_process(), input$selected_assay)
        })

        server_module_pca_box(
            id = "features",
            single_assay = single_assay,
            method = input$selected_method,
            transpose = FALSE
        )
        server_module_pca_box(
            id = "samples",
            single_assay = single_assay,
            method = input$selected_method,
            transpose = TRUE
        )
    })
}

#' PCA Box server module
#'
#' @param id module id
#' @param single_assay a reactiveVal that contains the selected assay
#' @param method a character string specifying the PCA method to use
#' valid values are c("nipals")
#' @param transpose a boolean that specifies if the PCA should be transposed
#' TRUE for samples and FALSE for features
#' @return A shiny module server function that contains the PCA logic
#' @rdname INTERNAL_server_module_pca_box
#' @keywords internal
#'
server_module_pca_box <- function(id, single_assay, method, transpose) {
    moduleServer(id, function(input, output, session) {
        output$pca <- renderPlotly({
            pca <- pcaMethods_wrapper(
                single_assay(),
                method = method,
                transpose = transpose
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

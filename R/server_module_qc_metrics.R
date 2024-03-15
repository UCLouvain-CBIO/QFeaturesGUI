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
        single_assay <- eventReactive(input$selected_assay, {
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
#' @importFrom shiny moduleServer observe req reactive
#' @importFrom ggplot2 ggplot aes geom_point xlab ylab
#' @importFrom plotly ggplotly renderPlotly layout
#' @importFrom SummarizedExperiment colData rowData
#'
server_module_pca_box <- function(id, single_assay, method, transpose) {
    moduleServer(id, function(input, output, session) {
        annotation_names <- reactive({
            req(single_assay())
            if (id == "features") {
                colnames(rowData(single_assay()))
            } else {
                colnames(colData(single_assay()))
            }
        })

        observe({
            req(single_assay())
            req(annotation_names())
            stopifnot(is(single_assay(), "SummarizedExperiment"))
            updateSelectInput(session,
                "pca_color",
                choices = annotation_names()
            )
        })

        color_data <- reactive({
            req(single_assay())
            req(input$pca_color)
            if (id == "features") {
                df <- rowData(single_assay())[, input$pca_color, drop = FALSE]
            } else {
                df <- colData(single_assay())[, input$pca_color, drop = FALSE]
            }
            colnames(df) <- "color"
            return(df)
        })

        pca_result <- reactive({
            req(single_assay())
            pcaMethods_wrapper(
                single_assay(),
                method = method,
                transpose = transpose
            )
        })
        df <- reactive({
            req(pca_result())
            req(color_data())
            print(color_data())
            merge(
                data.frame(scores(pca_result())),
                color_data(),
                by = "row.names"
            )
        })

        output$pca <- renderPlotly({
            req(df())
            # TODO: Add color (e.g. cell type), annotation.
            # TODO: Add a table with the selected points.
            print(df())
            plot <- ggplot(
                df(),
                aes(x = PC1, y = PC2, color = color, text = Row.names)
            ) +
                geom_point() +
                xlab(paste(
                    "PC1", round(pca_result()@R2[1] * 100, 2),
                    "% of the variance"
                )) +
                ylab(paste(
                    "PC2", round(pca_result()@R2[2] * 100, 2),
                    "% of the variance"
                ))+
                theme(legend.position = c(0.8, 0.2))

                ggplotly(plot, tooltip = "text", dynamicTicks = TRUE)

            # Find a way to make the legend readable (legend box ?)
            # change the color argument to the annotation name
            # some annotation cause errors

        })
    })
}

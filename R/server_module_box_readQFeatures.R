#' A server module that contains the server logic to the readQFeatures box module
#'
#' @param id module id
#' @param input_table a reactiveVal containing the input table
#' @param sample_table a reactiveVal containing the sample table
#'
#' @return A QFeatures object
#' @rdname INTERNAL_box_readqfeatures_server
#' @keywords internal
#'
#' @importFrom shiny is.reactive reactive moduleServer observe eventReactive updateSelectInput removeModal downloadHandler
#' @importFrom DT renderDataTable datatable
#' @importFrom QFeatures readQFeatures
#' @importFrom QFeatures zeroIsNA
#' @importFrom methods as
#' @import SingleCellExperiment
#' @import SummarizedExperiment
#' @import MultiAssayExperiment
#'
box_readqfeatures_server <- function(id, input_table, sample_table) {
    stopifnot(is.reactive(input_table))
    stopifnot(is.reactive(sample_table))

    moduleServer(id, function(input, output, session) {
        qfeatures <- eventReactive(input$convert, {
            loading(paste("Be aware that this operation",
                "can be quite time consuming for large data sets",
                sep = " "
            ))
            if (is.data.frame(sample_table())) {
                if (input$run_col != "NULL") {
                    qfeatures <- error_handler(
                        QFeatures::readQFeatures,
                        component_name = "QFeatures converting (readQFeatures)",
                        assayData = input_table(),
                        colData = sample_table(),
                        runCol = input$run_col,
                        removeEmptyCols = input$removeEmptyCols,
                        verbose = FALSE
                    )
                } else {
                    qfeatures <- error_handler(
                        QFeatures::readQFeatures,
                        component_name = "QFeatures converting (readQFeatures)",
                        assayData = input_table(),
                        colData = sample_table(),
                        runCol = NULL,
                        removeEmptyCols = input$removeEmptyCols,
                        verbose = FALSE
                    )
                }
            } else {
                if (input$run_col != "NULL"){
                  qfeatures <- error_handler(
                    QFeatures::readQFeatures,
                    component_name = "QFeatures converting (readQFeatures)",
                    assayData = input_table(),
                    runCol = input$run_col,
                    quantCols = input$quant_cols,
                    removeEmptyCols = input$removeEmptyCols,
                    verbose = FALSE
                  )
                } else {
                  qfeatures <- error_handler(
                    QFeatures::readQFeatures,
                    component_name = "QFeatures converting (readQFeatures)",
                    assayData = input_table(),
                    runCol = NULL,
                    quantCols = input$quant_cols,
                    removeEmptyCols = input$removeEmptyCols,
                    verbose = FALSE
                  )
                }
            }
            if (input$zero_as_NA && length(qfeatures) > 0) {
                qfeatures <- error_handler(
                    QFeatures::zeroIsNA,
                    component_name = "QFeatures converting (zero_as_NA)",
                    object = qfeatures,
                    i = seq_along(qfeatures)
                )
            }
            if (input$logTransform) {
                qfeatures <- error_handler(
                    QFeatures::logTransform,
                    component_name = "Log transforming QFeatures",
                    object = qfeatures,
                    i = seq_along(qfeatures),
                    base = 2,
                    name = paste0(names(qfeatures), "_logTransformed")
                )
            }
            if (input$singlecell) {
                el <- ExperimentList(lapply(
                    experiments(qfeatures),
                    as, "SingleCellExperiment"
                ))
                experiments(qfeatures) <- el
            }
            # The following code is a workaround
            # to fix keep track of the steps in the QFeatures object
            # The idea is that each page will be assigned a number,
            # and will use the assays that have the number - 1 in the name.
            # And then add assays with the number of the page in the QFeatures
            for (i in seq_along(qfeatures)) {
                names(qfeatures)[i] <- paste0(
                    names(qfeatures)[i],
                    "_(QFeaturesGUI#0)_initial_import"
                )
            }
            removeModal()
            qfeatures
        })

        observe({
            updateSelectInput(session,
                "run_col",
                choices = c("NULL", colnames(input_table())),
                selected = "NULL"
            )
            updateSelectInput(session,
                "quant_cols",
                choices = colnames(input_table())
            )
        })

        qfeatures_df <- reactive({
            req(qfeatures())
            error_handler(
                qfeatures_to_df,
                component_name = "qfeatures_to_df",
                page_assays_subset(qfeatures(), "_(QFeaturesGUI#0)")
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
                    data.frame(assay(qfeatures()[[row]])),
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

        output$downloadQFeatures <- downloadHandler(
            filename = function() {
                "qfeatures_object.rds"
            },
            content = function(file) {
                final_qfeatures <- qfeatures()
                names(final_qfeatures) <- remove_QFeaturesGUI(names(final_qfeatures))
                saveRDS(final_qfeatures, file)
            }
        )
    })
}

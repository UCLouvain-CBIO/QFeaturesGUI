#' A server module that contains the server logic to the readscp box module
#'
#' @param id module id
#' @param input_table a reactiveVal containing the input table
#' @param sample_table a reactiveVal containing the sample table
#'
#' @return A QFeatures object
#' @rdname INTERNAL_box_readscp_server
#' @keywords internal
#'
#' @importFrom shiny is.reactive reactive moduleServer observe eventReactive updateSelectInput removeModal downloadHandler
#' @importFrom DT renderDataTable datatable
#' @importFrom scp readSCP
#' @importFrom QFeatures zeroIsNA
#' @importFrom SummarizedExperiment assay
#'
box_readscp_server <- function(id, input_table, sample_table) {
    stopifnot(is.reactive(input_table))
    stopifnot(is.reactive(sample_table))

    moduleServer(id, function(input, output, session) {
        observeEvent(input$convert, {
            loading(paste("Be aware that this operation",
                "can be quite time consuming for large data sets",
                sep = " "
            ))
            global_rv$qfeatures <- error_handler(
                scp::readSCP,
                component_name = "QFeatures converting (readSCP)",
                featureData = input_table(),
                colData = sample_table(),
                batchCol = input$batch_col,
                channelCol = input$channel_col,
                removeEmptyCols = input$removeEmptyCols,
                verbose = FALSE
            )
            if (input$zero_as_NA && length(global_rv$qfeatures) > 0) {
                global_rv$qfeatures <- error_handler(
                    QFeatures::zeroIsNA,
                    component_name = "QFeatures converting (zero_as_NA)",
                    object = global_rv$qfeatures,
                    i = seq_along(global_rv$qfeatures)
                )
            }
            # The following code is a workaround
            # to fix keep track of the steps in the QFeatures object
            # The idea is that each page will be assigned a number,
            # and will use the assays that have the number - 1 in the name.
            # And then add assays with the number of the page in the QFeatures
            global_rv$initial_PSM_names <- names(global_rv$qfeatures)
            for (i in seq_along(global_rv$qfeatures)) {
                names(global_rv$qfeatures)[i] <- paste0(
                    names(global_rv$qfeatures)[i],
                    "_(scpGUI#0)"
                )
            }
            removeModal()
        })

        observe({
            input$reload_button
            updateSelectInput(session,
                "batch_col",
                choices = colnames(sample_table())
            )
            updateSelectInput(session,
                "channel_col",
                choices = colnames(sample_table())
            )
        })

        qfeatures_df <- reactive({
            error_handler(
                qfeatures_to_df,
                component_name = "qfeatures_to_df",
                page_assays_subset(global_rv$qfeatures, "_(scpGUI#0)")
            )
        })
        output$download_qfeatures <- downloadHandler(
            filename = function() {
                "scp_qfeature_object.rds"
            },
            content = function(file) {
                saveRDS(global_rv$qfeatures, file)
            }
        )
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
    })
}

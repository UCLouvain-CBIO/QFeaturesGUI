#' @title Server logic for the missing values tab
#' @param id The module id
#' @param step_number The step number
#' @param type the type of data to deal missing values with (e.g. features or samples)
#'
#' @return The processed assays
#' @rdname INTERNAL_server_module_missing_values_tab
#' @keywords internal
#'
#' @importFrom shiny moduleServer eventReactive observeEvent renderUI reactiveValues
#' @importFrom shiny observe reactiveValuesToList NS reactive updateSelectInput plotOutput
#' @importFrom shinydashboard renderInfoBox
#' @importFrom shinycssloaders showPageSpinner hidePageSpinner
#' @importFrom QFeatures nNA filterNA rbindRowData rowDataNames
#' @importFrom DT renderDataTable dataTableOutput datatable
#' @importFrom SingleCellExperiment colData
#' @importFrom ggplot2 ggplot
server_module_missing_values_tab <- function(id, step_number, type) {
    moduleServer(id, function(input, output, session) {
        assays_to_process <- eventReactive(input$reload, {
            error_handler(page_assays_subset,
                component_name = "Page assays subset",
                qfeatures = global_rv$qfeatures,
                pattern = paste0("_(QFeaturesGUI#", step_number - 1, ")")
            )
        })

        qf_with_NA <- reactive({
            req(assays_to_process())
            tableNA <- nNA(
                object = assays_to_process(),
                i = seq_along(assays_to_process()),
                addToObject = TRUE
            )
            if (type == "features") {
                updateSelectInput(
                    session = session,
                    inputId = paste0("pca_color_", type),
                    choices = c("NULL", rowDataNames(tableNA)[[1]]),
                    selected = "NULL"
                )
            } else {
                updateSelectInput(
                    session = session,
                    inputId = paste0("pca_color_", type),
                    choices = c("NULL", names(colData(tableNA))),
                    selected = "NULL"
                )
            }

            tableNA
        })

        output[[paste0("dynamic_", type)]] <- renderUI({
            req(qf_with_NA())
            if (type == "features") {
                plotOutput(NS(id, paste0("plot_na_", type)))
            } else {
                if (length(rownames(colData(qf_with_NA()))) > 10) {
                    plotOutput(NS(id, paste0("plot_na_", type)))
                } else {
                    DT::dataTableOutput(NS(id, paste0("dataTable_na_", type)))
                }
            }
        })

        tablePlot <- reactive({
            req(qf_with_NA())

            if (type == "features") {
                tablePlot <- rbindRowData(qf_with_NA(), i = seq_along(qf_with_NA()))
            } else {
                tablePlot <- colData(qf_with_NA())
            }
            tablePlot
        })
        output[[paste0("dataTable_na_", type)]] <- DT::renderDataTable({
            req(tablePlot())
            DT::datatable(
                as.data.frame(tablePlot()),
                options = list(scrollX = TRUE)
            )
        })
        output[[paste0("nb_removed_", type)]] <- renderInfoBox({
            req(tablePlot())
            nbRemoved <- sum(tablePlot()$pNA >= input[[paste0("threshold_", type)]])
            infoBox(paste0("Number of ", type, " removed: "), nbRemoved, fill = TRUE, color = "light-blue", icon = icon("filter"))
        })
        output[[paste0("percent_removed_", type)]] <- renderInfoBox({
            req(tablePlot())
            nbRemoved <- sum(tablePlot()$pNA >= input[[paste0("threshold_", type)]])
            percent <- round(nbRemoved / length(tablePlot()$pNA) * 100, digits = 1)
            infoBox(paste0("Percent of ", type, " removed: "), paste(percent, "%"), fill = TRUE, color = "light-blue", icon = icon("percent"))
        })

        plotNA <- reactive({
            if (input[[paste0("pca_color_", type)]] == "NULL") {
                color <- NULL
            } else {
                color <- tablePlot()[[input[[paste0("pca_color_", type)]]]]
            }
            ggplot(tablePlot()) +
                geom_histogram(
                    aes(
                        x = pNA,
                        fill = color
                    ),
                    show.legend = input[[paste0("show_legend_", type)]],
                    binwidth = 0.01,
                    boundary = 20,
                    closed = "right"
                ) +
                scale_x_continuous(
                    limits = c(0, 1),
                    expand = c(0, 0)
                )
        })
        output[[paste0("plot_na_", type)]] <- renderPlot({
            plotNA() +
                geom_vline(
                    xintercept = input[[paste0("threshold_", type)]],
                    colour = "red"
                ) +
                annotate(
                    "rect",
                    xmin = input[[paste0("threshold_", type)]],
                    xmax = Inf,
                    ymin = -Inf,
                    ymax = Inf,
                    alpha = .5
                )
        })

        observeEvent(input$export, {
            req(qf_with_NA())
            shinycssloaders::showPageSpinner(
                type = "6",
                caption = "The filtering of QFeatures object can be quite time consuming for large datasets"
            )
            if (type == "features") {
                processed_assays <- QFeatures::filterNA(qf_with_NA(),
                    i = seq_along(qf_with_NA()),
                    pNA = input[[paste0("threshold_", type)]]
                )
            } else {
                processed_assays <- qf_with_NA()[, qf_with_NA()$pNA >= input[[paste0("threshold_", type)]], ]
            }

            error_handler(
                add_assays_to_global_rv,
                component_name = "Add assays to global_rv",
                processed_qfeatures = processed_assays,
                step_number = step_number,
                type = "features_filtering"
            )
            shinycssloaders::hidePageSpinner()
        })
    })
}

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
#' @importFrom shiny observe reactiveValuesToList NS reactive updateSelectInput plotOutput isolate
#' @importFrom shinydashboard renderInfoBox
#' @importFrom QFeatures nNA filterNA rbindRowData rowDataNames
#' @importFrom DT renderDataTable dataTableOutput datatable
#' @importFrom SingleCellExperiment colData
#' @importFrom ggplot2 ggplot aes geom_histogram geom_vline scale_x_continuous annotate
server_module_missing_values_tab <- function(id, step_number, type, step_rv, parent_rv) {
    moduleServer(id, function(input, output, session) {
        pattern <- paste0("_(QFeaturesGUI#", step_number - 1, ")")

        step_ready <- reactive({
            if (!is.null(parent_rv)) req(parent_rv() > 0L)
            TRUE
        })

        parent_assays <- reactive({
            req(step_ready())
            error_handler(page_assays_subset,
                component_name = "Page assays subset",
                qfeatures = .qf$qfeatures,
                pattern = pattern
            )
        })

        tableMetadataNA <- reactive({
            req(parent_assays())
            tableNA <- nNA(
                object = parent_assays(),
                i = seq_along(parent_assays())
            )
            if (type == "features") {
                df_to_render <- rbindRowData(parent_assays(), seq_along(parent_assays()))
                df_to_render <- merge(df_to_render, y = tableNA$nNArows, by = c("rowname", "assay"), by.x = c("rowname", "assay"), by.y = c("name", "assay"))
                updateSelectInput(
                    session = session,
                    inputId = paste0("pca_color_", type),
                    choices = c("NULL", names(df_to_render)),
                    selected = "NULL"
                )
            } else {
                df_to_render <- colData(parent_assays())
                df_to_render$pNA <- tableNA$nNAcols$pNA[match(rownames(df_to_render), tableNA$nNAcols$name)]
                updateSelectInput(
                    session = session,
                    inputId = paste0("pca_color_", type),
                    choices = c("NULL", names(df_to_render)),
                    selected = "NULL"
                )
            }
            df_to_render
        })

        output[[paste0("dynamic_", type)]] <- renderUI({
            req(tableMetadataNA())
            if (type == "features") {
                with_output_waiter(
                    plotOutput(NS(id, paste0("plot_na_", type))),
                    html = waiter::spin_6(),
                    color = "transparent"
                )
            } else {
                if (length(rownames(tableMetadataNA())) > 10) {
                    with_output_waiter(
                        plotOutput(NS(id, paste0("plot_na_", type))),
                        html = waiter::spin_6(),
                        color = "transparent"
                    )
                } else {
                    DT::dataTableOutput(NS(id, paste0("dataTable_na_", type)))
                }
            }
        })

        output[[paste0("dataTable_na_", type)]] <- DT::renderDataTable({
            req(tableMetadataNA())
            DT::datatable(
                as.data.frame(tableMetadataNA()),
                options = list(scrollX = TRUE)
            )
        })
        output[[paste0("nb_removed_", type)]] <- renderInfoBox({
            req(tableMetadataNA())
            nbRemoved <- sum(tableMetadataNA()$pNA >= input[[paste0("threshold_", type)]], na.rm = TRUE)
            infoBox(paste0("Number of ", type, " removed: "), nbRemoved, fill = TRUE, color = "light-blue", icon = icon("filter"))
        })
        output[[paste0("percent_removed_", type)]] <- renderInfoBox({
            req(tableMetadataNA())
            nbRemoved <- sum(tableMetadataNA()$pNA >= input[[paste0("threshold_", type)]], na.rm = TRUE)
            percent <- round(nbRemoved / length(tableMetadataNA()$pNA) * 100, digits = 1)
            infoBox(paste0("Percent of ", type, " removed: "), paste(percent, "%"), fill = TRUE, color = "light-blue", icon = icon("percent"))
        })

        plotNA <- reactive({
            if (input[[paste0("pca_color_", type)]] == "NULL") {
                color <- NULL
            } else {
                color <- tableMetadataNA()[[input[[paste0("pca_color_", type)]]]]
            }
            ggplot(as.data.frame(tableMetadataNA())) +
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

        observeEvent(input$export,
            {
                req(tableMetadataNA())
                with_task_loader(
                    caption = "The filtering of QFeatures object can be quite time consuming for large datasets",
                    expr = {
                        if (type == "features") {
                            processed_assays <- QFeatures::filterNA(parent_assays(),
                                i = seq_along(parent_assays()),
                                pNA = input[[paste0("threshold_", type)]]
                            )
                        } else {
                            processed_assays <- parent_assays()[, tableMetadataNA()$pNA <= input[[paste0("threshold_", type)]], ]
                        }
                        error_handler(
                            add_assays_to_global_rv,
                            component_name = "Add assays to global_rv",
                            processed_qfeatures = processed_assays,
                            step_number = step_number,
                            type = paste0("missing_value_", type)
                        )
                        global_rv$code_lines[[paste0("Initialization_names_", step_number)]] <- codeGeneratorInitialization(qf = .qf$qfeatures, step_number = step_number)
                        global_rv$code_lines[[paste0("Missing_values_",type,"_",step_number)]] <- codeGeneratorNA(qf = .qf$qfeatures, pNA = input[[paste0("threshold_", type)]], type = type, step_number = step_number)
                        step_rv(step_rv() + 1L)
                    }
                )
            },
            ignoreInit = TRUE
        )
    })
}

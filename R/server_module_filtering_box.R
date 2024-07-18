#' @title server_module_annotation_plot
#'
#' @param id The module id
#' @param assays_to_process A reactiveVal that contains the different assays that will be used in the module
#' @param type A character string specifying the type of the filtering (sample or feature based)
#' @param state A list that contains the state of the module (input states)
#' @return The updated assays
#' @rdname INTERNAL_server_module_filtering_box
#' @keywords internal
#'
#' @importFrom shiny moduleServer updateSelectInput reactive observe is.reactive req updateTextInput updateSelectInput
#' @importFrom SummarizedExperiment colData rowData
#' @importFrom shinyFeedback feedbackDanger
#' @importFrom QFeatures filterFeatures
#'
server_module_filtering_box <- function(id, assays_to_process, type, state) {
    is.reactive(assays_to_process)
    moduleServer(id, function(input, output, session) {
        annotations_names <- reactive({
            req(assays_to_process())
            if (type == "samples") {
                colnames(colData(assays_to_process()))
            } else if (type == "features") {
                colnames(rowData(assays_to_process()[[1]]))
            }
        })
        observe({
            updateSelectInput(
                inputId = "annotation_selection",
                choices = as.character(annotations_names())
            )
        })
        if (!is.null(state)) {
            updateSelectInput(
                inputId = "filter_operator",
                selected = state$filter_operator
            )

            updateTextInput(
                inputId = "filter_value",
                value = state$filter_value
            )

            observe({
                updateSelectInput(
                    inputId = "annotation_selection",
                    choices = as.character(annotations_names()),
                    selected = state$annotation_selection
                )
            })
        }


        annotations_type <- reactive({
            req(input$annotation_selection)
            req(assays_to_process())
            if (type == "samples") {
                typeof(colData(assays_to_process())[[input$annotation_selection]])
            } else if (type == "features") {
                typeof(rowData(assays_to_process()[[1]])[[input$annotation_selection]])
            }
        })
        clean_filter_value <- reactive({
            req(annotations_type())
            num_val <- suppressWarnings(as.numeric(input$filter_value))
            num <- (annotations_type() == "numeric" | annotations_type() == "integer") && !is.na(num_val)
            char <- (annotations_type() == "character" | annotations_type() == "factor")

            feedbackDanger("filter_value",
                show = !(num | char),
                paste0(
                    "The type of the filter value should",
                    " coincide with the type of the annotation.",
                    "<br>",
                    "Annotation type: ", annotations_type(),
                    "<br>",
                    "Filter type: ", typeof(input$filter_value)
                )
            )
            if (num) {
                return(num_val)
            } else {
                return(input$filter_value)
            }
        })
        server_module_annotation_plot(
            "annotation_plot",
            assays_to_process,
            type,
            clean_filter_value,
            reactive({
                input$annotation_selection
            }),
            reactive({
                input$filter_operator
            })
        )
        condition <- reactive({
            filter_value <- clean_filter_value()
            if (is.character(filter_value)) {
                filter_value <- paste0("\"", filter_value, "\"")
            }
            paste(
                input$annotation_selection,
                input$filter_operator,
                filter_value
            )
        })

        return(list(
            condition = condition,
            annotation_selection = reactive(input$annotation_selection),
            filter_operator = reactive(input$filter_operator),
            filter_value = reactive(input$filter_value)
        ))
    })
}


#' @title Annotation Plot server module
#'
#' @param id The module id
#' @param assays_to_process A reactiveVal that contains the different assays that will be used in the module
#' @param type A character string specifying the type of the filtering (sample or feature based)
#' @param filter_value The value that will be used in combinaison with the filter operator
#' @param annotation The annotation that will be used to filter the data
#' @param filter_operator The operator that will be used to filter the data
#'
#' @return A plot of the distribution of the selected annotation in a specific assay
#' @rdname INTERNAL_server_module_annotation_plot
#' @keywords internal
#'
#' @importFrom shiny moduleServer observe req eventReactive reactive
#' @importFrom plotly plot_ly renderPlotly
#' @importFrom shinyBS createAlert closeAlert
#'
server_module_annotation_plot <- function(
        id,
        assays_to_process,
        type,
        filter_value,
        selected_annotation,
        filter_operator) {
    moduleServer(id, function(input, output, session) {
        observe({
            req(assays_to_process())
            updateSelectInput(
                inputId = "selected_assay",
                choices = names(assays_to_process())
            )
        })
        single_assay <- reactive({
            req(assays_to_process())
            req(input$selected_assay)
            # Warning appears here
            # Warning message: 'experiments' dropped; see 'drops()'
            # see with Chris
            getWithColData(assays_to_process(), input$selected_assay)
        })
        annotation_values <- reactive({
            req(single_assay())
            if (type == "samples") {
                as.data.frame(colData(single_assay()))[[selected_annotation()]]
            } else if (type == "features") {
                as.data.frame(rowData(single_assay()))[[selected_annotation()]]
            }
        })
        filtered_annotation <- reactive({
            req(annotation_values())
            operator <- get(filter_operator())
            subset(
                annotation_values(),
                operator(
                    annotation_values(),
                    filter_value()
                )
            )
        })
        observe({
            req(annotation_values())
            if (length(filtered_annotation()) == 0) {
                createAlert(session,
                    anchorId = "alert",
                    alertId = "alert_filter",
                    title = "Warning",
                    content = "With the selected filtering parameters, no data will be remaining in this assay.",
                    append = FALSE,
                    style = "warning"
                )
            } else {
                closeAlert(session, "alert_filter")
            }
            output$plot <- renderPlotly({
                error_handler(annotation_plot_wrapper,
                    "annotation_plot (filtering_box)",
                    annotation = annotation_values(),
                    filtered_annotation = filtered_annotation(),
                    assay_name = input$selected_assay,
                    annotation_name = selected_annotation()
                )
            })
        })
    })
}

#' @title Annotation plot wrapper
#'
#' @param annotation_df A data.frame that contains the annotation values
#' @param selected_annotation A character string that contains the name of the selected annotation
#' @param filter_value A character string that contains the value of the filter
#' @return A plotly object
#'
#' @rdname INTERNAL_annotation_plot
#' @keywords internal
#'
#' @importFrom plotly plot_ly config %>% add_histogram layout add_annotations
#'
annotation_plot_wrapper <- function(
        annotation,
        filtered_annotation,
        assay_name,
        annotation_name) {
    if (all(is.na(annotation))) {
        plot <- plot_ly(
            x = numeric(0),
            y = numeric(0),
            type = "scatter",
            mode = "markers"
        ) %>%
            add_annotations(
                text = "All values are NA, cannot create histogram.",
                xref = "paper",
                yref = "paper",
                x = 0.5,
                y = 0.5,
                showarrow = FALSE
            ) %>%
            layout(
                showlegend = FALSE,
                xaxis = list(
                    showticklabels = FALSE,
                    zeroline = FALSE,
                    showgrid = FALSE
                ),
                yaxis = list(
                    showticklabels = FALSE,
                    zeroline = FALSE,
                    showgrid = FALSE
                )
            )
        return(plot)
    }
    plot <- plot_ly() %>%
        add_histogram(x = annotation, name = "Before Filtering") %>%
        layout(
            barmode = "group",
            xaxis = list(title = paste0("Annotation Values (", annotation_name, ")")),
            yaxis = list(title = "Number of Appearance"),
            title = assay_name
        ) %>%
        config(displaylogo = FALSE, toImageButtonOptions = list(
            format = "svg",
            filename = "annotation_plot",
            height = 500,
            width = 700,
            scale = 1
        ))
    if (length(filtered_annotation) > 0) {
        plot <- plot %>%
            add_histogram(x = filtered_annotation, name = "After Filtering")
    }
    return(plot)
}

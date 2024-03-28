#' @title server_module_annotation_plot
#'
#' @param id The module id
#' @param qfeat A reactiveVal that contains the different assays that will be used in the module
#' @param type A character string specifying the type of the filtering (sample or feature based)
#' @return The updated assays
#' @rdname INTERNAL_server_module_filtering_box
#' @keywords internal
#'
#' @importFrom shiny moduleServer updateSelectInput reactive observe is.reactive req
#' @importFrom SummarizedExperiment colData rowData
#' @importFrom shinyFeedback feedbackDanger
#'
server_module_filtering_box <- function(id, qfeat, type) {
    is.reactive(qfeat)
    moduleServer(id, function(input, output, session) {
        annotations_names <- reactive({
            req(qfeat())
            if (type == "samples") {
                colnames(colData(qfeat()[[1]]))
            } else if (type == "features") {
                colnames(rowData(qfeat()[[1]]))
            }
        })
        observe({
            updateSelectInput(
                inputId = "annotation_selection",
                choices = as.character(annotations_names())
            )
        })
        annotations_type <- reactive({
            req(qfeat())
            if (type == "samples") {
                typeof(colData(qfeat()[[1]])[[input$annotation_selection]])
            } else if (type == "features") {
                typeof(rowData(qfeat()[[1]])[[input$annotation_selection]])
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
            qfeat,
            type,
            clean_filter_value,
            reactive({
                input$annotation_selection
            }),
            reactive(
                {
                    input$filter_operator
                }
            ),
            parent_session = session
        )
    })
}


#' @title Annotation Plot server module
#'
#' @param id The module id
#' @param qfeat A reactiveVal that contains the different assays that will be used in the module
#' @param type A character string specifying the type of the filtering (sample or feature based)
#' @param filter_value The value that will be used in combinaison with the filter operator
#' @param annotation The annotation that will be used to filter the data
#' @param filter_operator The operator that will be used to filter the data
#' @param parent_session The parent session
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
        qfeat,
        type,
        filter_value,
        selected_annotation,
        filter_operator,
        parent_session) {
    moduleServer(id, function(input, output, session) {
        observe({
            req(qfeat())
            updateSelectInput(
                inputId = "selected_assay",
                choices = names(qfeat())
            )
        })
        single_assay <- eventReactive(input$selected_assay, {
            req(qfeat())
            req(input$selected_assay)
            # Warning appears here
            # Warning message: 'experiments' dropped; see 'drops()'
            # see with Chris
            getWithColData(qfeat(), input$selected_assay)
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
            if(length(filtered_annotation()) == 0){
                createAlert(session, "alert",
                alertId = "alert_filter",
                 title = "Warning",
                 content = "With the selected filtering parameters, no data will be remaining in this assay.",
                 append = FALSE,
                 style = "warning")
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
#' @importFrom plotly plot_ly config %>% add_histogram layout
annotation_plot_wrapper <- function(
        annotation,
        filtered_annotation,
        assay_name,
        annotation_name) {
    plot <- plot_ly() %>%
        add_histogram(x = annotation, name = "Before Filtering") %>%
        layout(barmode = "group",
            xaxis = list(title = paste0("Annotation Values (", annotation_name, ")")),
            yaxis = list(title = "Number of Appearance"),
            title = assay_name) %>%
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

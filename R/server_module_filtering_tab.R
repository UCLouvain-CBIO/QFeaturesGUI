#' @title Server logic for the filtering tab
#' @param id The module id
#' @param step_number The step number
#' @param step_rv A reactiveVal used to signal when this step is saved
#' @param parent_rv A reactiveVal used to gate this step until the parent step is saved
#' @param type A character string specifying the type of the filtering (`"samples"` or `"features"`)
#'
#' @return The processed assays
#' @rdname INTERNAL_server_module_filtering_tab
#' @keywords internal
#'
#' @importFrom shiny moduleServer eventReactive observeEvent renderUI reactiveValues observe reactiveValuesToList NS reactive req reactiveVal removeModal icon
#' @importFrom QFeatures filterFeatures
#' @importFrom htmltools tags
#' @importFrom shinydashboard renderInfoBox infoBox
server_module_filtering_tab <- function(
      id,
      step_number,
      step_rv,
      parent_rv,
      type = c("samples", "features")
) {
    type <- match.arg(type)

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

        server_module_qc_metrics("psm_pre", parent_assays)

        n_boxes <- reactiveVal(0)

        filtering_conditions <- reactiveValues()
        boxes_states <- reactiveValues()
        observeEvent(input$add_box, {
            n_boxes(n_boxes() + 1)
        })

        observeEvent(input$remove_box, {
            if (n_boxes() > 0) {
                n_boxes(n_boxes() - 1)
            }
        })

        observeEvent(n_boxes(), {
            output$filtering_boxes <- renderUI({
                if (n_boxes() > 0) {
                    lapply(seq_len(n_boxes()), function(i) {
                        interface_module_filtering_box(
                            NS(id, paste0("filtering_", i)),
                            box_title = paste0("Filtering Box #", i)
                        )
                    })
                }
            })
            if (n_boxes() > 0) {
                lapply(seq_len(n_boxes()), function(i) {
                    res <- server_module_filtering_box(
                        paste0("filtering_", i),
                        parent_assays,
                        type,
                        boxes_states[[paste0("box_", i)]]
                    )

                    observe({
                        filtering_conditions[[paste0("condition_", i)]] <- res$condition()
                        boxes_states[[paste0("box_", i)]] <- list(
                            annotation_selection = res$annotation_selection(),
                            filter_operator = res$filter_operator(),
                            filter_value = res$filter_value()
                        )
                    })
                })
            }

            output$boxes_summary <- renderUI({
                tags$div(
                    tags$p(tags$b(paste0("Number of boxes: ", n_boxes()))),
                    tags$ul(
                        lapply(seq_len(n_boxes()), function(i) {
                            tags$li(
                                style = "font-size: 20px;",
                                class = "list-element",
                                tags$span(
                                    style = "font-size: 15px",
                                    paste0("Filtering Box #", i, ":")
                                ),
                                tags$br(),
                                tags$span(
                                    style = "margin-left: 20px; font-size: 13px;",
                                    paste0(
                                        "Annotation Used: ",
                                        boxes_states[[paste0("box_", i)]]$annotation_selection
                                    )
                                )
                            )
                        }),
                        class = "list-group"
                    )
                )
            })

            output$filtering_summary <- renderUI({
                if (n_boxes() > 0) {
                    tags$ul(
                        lapply(seq_len(n_boxes()), function(i) {
                            tags$li(
                                class = "list-element",
                                style = "font-size: 20px;",
                                tags$span(
                                    style = "font-size: 14px;",
                                    paste0("Filtering Condition #", i, ":"),
                                    tags$br(),
                                    tags$span(
                                        style = "margin-left: 20px;",
                                        tags$b(
                                            filtering_conditions[[paste0("condition_", i)]]
                                        )
                                    )
                                )
                            )
                        }),
                        class = "list-group"
                    )
                }
            })
        })
        filtering_conditions_list <- reactive({
            reactiveValuesToList(filtering_conditions)
        })
        entire_condition <- reactive({
            res <- lapply(seq_len(n_boxes()), function(index) {
                filtering_conditions_list()[[index]]
            })
            res <- unlist(res)
            if (length(res) == 0) {
                return(NULL)
            }
            if (type == "samples") {
                return(paste(paste0("qfeatures$", res), collapse = " & "))
            }
            as.formula(paste0("~", paste(res, collapse = " & ")))
        })

        processed_assays <- eventReactive(
            c(input$apply_filters, step_ready()),
            {
                if (length(filtering_conditions_list()) > 0) {
                    if (type == "samples") {
                        return(error_handler(sample_filtering,
                            component_name = "Sample filtering",
                            qfeatures = parent_assays(),
                            conditions = entire_condition()
                        ))
                    }
                    return(error_handler(filterFeatures,
                        component_name = "Filter features",
                        object = parent_assays(),
                        filter = entire_condition()
                    ))
                }
                parent_assays()
            }
        )
        server_module_qc_metrics("psm_filtered", processed_assays)
        output[[paste0("number_", type, "_removed")]] <- renderInfoBox({
            nb_removed <- number_removed(parent_assays(), processed_assays(), type = type)
            infoBox(
                paste0("Number of ", type, " removed : "),
                nb_removed,
                fill = TRUE,
                color = "light-blue",
                icon = icon("filter")
            )
        })

        output[[paste0("percent_", type, "_removed")]] <- renderInfoBox({
            pct_removed <- percent_removed(parent_assays(), processed_assays(), type = type)
            infoBox(
                paste0("Percent of ", type, " removed : "),
                paste(pct_removed, "%"),
                fill = TRUE,
                color = "light-blue",
                icon = icon("filter")
            )
        })

        observeEvent(input$export, {
            req(processed_assays())
            loading(paste("Be aware that this operation",
                "can be quite time consuming for large data sets",
                sep = " "
            ))
            error_handler(
                add_assays_to_global_rv,
                component_name = "Add assays to global_rv",
                processed_qfeatures = processed_assays(),
                step_number = step_number,
                type = paste0(type, "_filtering")
            )
            step_rv(step_rv() + 1L)
            removeModal()
        }, ignoreInit = TRUE)
    })
}


#' @title sample filtering
#'
#' @param qfeatures A qfeatures object
#' @param conditions A string with the conditions to filter the samples (chr)
#'
#' @return A qfeatures object with the filtered samples
#' @rdname INTERNAL_sample_filtering
#' @keywords internal
#'
sample_filtering <- function(qfeatures, conditions) {
    qfeatures[, eval(parse(text = conditions)), ]
}

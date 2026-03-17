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
#' @importFrom shiny moduleServer eventReactive observeEvent renderUI reactiveValues observe NS reactive req reactiveVal removeModal icon
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
        filtering_condition_labels <- reactiveValues()
        boxes_states <- reactiveValues()

        display_or_default <- function(value, default) {
            if (is.null(value) || length(value) == 0 || !nzchar(as.character(value)[1])) {
                return(default)
            }
            as.character(value)[1]
        }

        observeEvent(input$add_box, {
            n_boxes(n_boxes() + 1)
        })

        observeEvent(input$remove_box, {
            if (n_boxes() > 0) {
                removed_index <- n_boxes()
                n_boxes(removed_index - 1L)
                filtering_conditions[[paste0("condition_", removed_index)]] <- NULL
                filtering_condition_labels[[paste0("condition_label_", removed_index)]] <- NULL
                boxes_states[[paste0("box_", removed_index)]] <- NULL
            }
        })

        observeEvent(n_boxes(), {
            output$filtering_boxes <- renderUI({
                if (n_boxes() == 0) {
                    return(tags$div(
                        class = "alert alert-warning",
                        style = "text-align: center; max-width: 680px; margin: 10px auto;",
                        "No filtering boxes yet. Add one from the configuration section."
                    ))
                }
                tags$div(
                    lapply(seq_len(n_boxes()), function(i) {
                        interface_module_filtering_box(
                            NS(id, paste0("filtering_", i)),
                            box_title = paste0("Filtering Box #", i),
                            width = 4
                        )
                    })
                )
            })

            if (n_boxes() > 0) {
                lapply(seq_len(n_boxes()), function(i) {
                    key <- paste0("box_", i)
                    res <- server_module_filtering_box(
                        paste0("filtering_", i),
                        parent_assays,
                        type,
                        boxes_states[[key]]
                    )
                    observe({
                        filtering_conditions[[paste0("condition_", i)]] <- res$condition()
                        filtering_condition_labels[[paste0("condition_label_", i)]] <- res$condition_label()
                        boxes_states[[key]] <- list(
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
                    if (n_boxes() == 0) {
                        tags$div(
                            class = "alert alert-warning",
                            style = "text-align: center; max-width: 680px; margin: 10px auto;",
                            "Add a filtering box to configure your first condition."
                        )
                    } else {
                        tags$div(
                            class = "list-group",
                            lapply(seq_len(n_boxes()), function(i) {
                                box_state <- boxes_states[[paste0("box_", i)]]
                                annotation_selection <- NULL
                                if (!is.null(box_state)) {
                                    annotation_selection <- box_state$annotation_selection
                                }
                                tags$div(
                                    class = "list-group-item",
                                    tags$div(
                                        style = "font-weight: 600;",
                                        paste0("Filtering Box #", i)
                                    ),
                                    tags$div(
                                        style = "margin-top: 4px; font-size: 13px;",
                                        paste0(
                                            "Annotation: ",
                                            display_or_default(annotation_selection, "Not selected yet")
                                        )
                                    )
                                )
                            })
                        )
                    }
                )
            })

            output$filtering_summary <- renderUI({
                if (n_boxes() == 0) {
                    return(tags$div(
                        class = "alert alert-warning",
                        style = "text-align: center; max-width: 680px; margin: 10px auto;",
                        "No filtering condition yet. Add at least one filtering box."
                    ))
                }

                tags$div(
                    class = "list-group",
                    lapply(seq_len(n_boxes()), function(i) {
                        condition_label <- display_or_default(
                            filtering_condition_labels[[paste0("condition_label_", i)]],
                            "Condition not configured yet."
                        )
                        tags$div(
                            class = "list-group-item",
                            tags$div(
                                style = "font-weight: 600;",
                                paste0("Condition #", i)
                            ),
                            tags$div(
                                style = "margin-top: 6px;",
                                tags$code(condition_label)
                            ),
                            if (i < n_boxes()) {
                                tags$div(
                                    style = "margin-top: 8px;",
                                    tags$span(class = "label label-default", "AND")
                                )
                            }
                        )
                    })
                )
            })
        })

        filtering_conditions_list <- reactive({
            conditions <- unlist(lapply(seq_len(n_boxes()), function(i) {
                filtering_conditions[[paste0("condition_", i)]]
            }), use.names = FALSE)
            conditions <- as.character(conditions)
            conditions[!is.na(conditions) & nzchar(conditions)]
        })
        entire_condition <- reactive({
            res <- filtering_conditions_list()
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

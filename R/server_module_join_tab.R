#' Server for the assay joining tab
#'
#' @param id module id
#' @return The server logic for the assay joining tab
#' @rdname INTERNAL_server_module_aggregation_tab
#' @keywords internal
#'
#' @importFrom shiny moduleServer updateSelectInput observeEvent eventReactive is.reactive
#' @importFrom MultiAssayExperiment getWithColData
#' @importFrom shinycssloaders hidePageSpinner showPageSpinner
#'
server_module_join_tab <- function(id, step_number, step_rv, parent_rv) {
  moduleServer(id, function(input, output, session) {
    pattern <- paste0("_(QFeaturesGUI#", step_number - 1, ")")
    
    step_ready <- reactive({
      if (!is.null(parent_rv)) req(parent_rv() > 0L)
      TRUE
    })
    
    assays_to_process <- reactive({
      error_handler(
        page_assays_subset,
        component_name = "Page assays subset",
        qfeatures = .qf$qfeatures,
        pattern = pattern
      )
    })
    
    ## Setup available rowdata variable names for providing
    ## fcol_join and fcol2_join
    rowdata_names <- reactive({
      req(assays_to_process())
      annotation_cols(assays_to_process(), "rowData")
    })
    observe({
      updateSelectInput(
        inputId = "fcol_join",
        ## Also allow selecting rownames
        choices = c("(row names)", as.character(rowdata_names()))
      )
    })
    observe({
      updateSelectInput(
        inputId = "fcol2_join",
        choices = c("(none)", as.character(rowdata_names()))
      )
    })
    
    ## Display available rownames
    observeEvent(assays_to_process(), {
      output$rownames <- renderUI({
        box(
          width = 12,
          height = "50%",
          p("If you want to join by row names, make sure the ",
            "the rownames below (partially) overlap:"),
          tags$div(
            tags$h4("Assays to join"),
            tags$ul(
              style = "width: 100%; height: 100%; overflow: auto",
              lapply(seq_along(assays_to_process()), function(i) {
                tags$li(
                  style = "font-size: 15px;",
                  class = "list-element",
                  tags$span(
                    style = "font-size: 15px",
                    names(assays_to_process())[i]
                  ),
                  tags$br(),
                  tags$span(
                    style = "margin-left: 20px; font-size: 13px;",
                    paste(
                      "Rownames: ",
                      paste(
                        head(rownames(assays_to_process())[[i]], 10),
                        collapse = ", "
                      ),
                      "[...]"
                    )
                  )
                )
              }),
              class = "list-group"
            )
          )
        )
      })
    })
    
    processed_assays <- reactiveVal()
    observeEvent(input$join, {
      shinycssloaders::showPageSpinner(
        type = 6,
        caption = "Be aware that joining can be quite time consuming for large data sets"
      )
      req(assays_to_process())
      req(input$fcol_join)
      req(input$fcol2_join)
      fcol2 <- input$fcol2_join
      if (input$fcol2_join == "(none)") fcol2 <- NULL
      if (input$fcol_join == "(row names)") {
        fcol <- fcol2 <- NULL
      } else {
        fcol <- input$fcol_join
      }
      processed_assays(error_handler(
        join_qfeatures,
        component_name = "join",
        qfeatures = assays_to_process(),
        fcol = fcol,
        fcol2 = fcol2
      ))
      shinycssloaders::hidePageSpinner()
    })
    
    observeEvent(input$export, {
      req(processed_assays())
      shinycssloaders::showPageSpinner(
        type = 6,
        caption = "Be aware that joining can be quite time consuming for large data sets"
      )
      error_handler(
        add_joined_assay_to_global_rv,
        component_name = "Add assays to global_rv",
        processed_qfeatures = processed_assays(),
        fcol = input$fcol_join,
        step_number = step_number,
        type = "join"
      )
      step_rv(step_rv() + 1L)
      shinycssloaders::hidePageSpinner()
    })
  })
}
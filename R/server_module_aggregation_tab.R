#' Server for the module aggregation tab
#'
#' @param id module id
#' @return The server logic for the aggregation tab
#' @rdname INTERNAL_server_module_aggregation_tab
#' @keywords internal
#'
#' @importFrom shiny moduleServer updateSelectInput observeEvent eventReactive is.reactive
#' @importFrom MultiAssayExperiment getWithColData
#'
server_module_aggregation_tab <- function(id, step_number, step_rv, parent_rv) {
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
    
    aggregation_names <- reactive({
      req(parent_assays())
      annotation_cols(parent_assays(), "rowData")
    })
    
    observe({
      updateSelectInput(
        inputId = "fcol",
        choices = as.character(aggregation_names())
      )
    })
    
    observe({
      updateSelectInput(
        inputId = "fun"
      )
    })
    
    
    processed_assays <- eventReactive(input$fcol, {
      req(parent_assays())
      error_handler(
        aggregation_qfeatures,
        component_name = "aggregation",
        qfeatures = parent_assays(),
        method = as.character(input$method),
        fcol = input$fcol
      )
    })
    
    output$density_plot <- renderPlotly({
      req(processed_assays())
      density_by_sample_plotly(
        qfeatures = processed_assays(),
        color = input$color
      )
    })
    
    observe({
      req(processed_assays())
      updateSelectInput(session,
                        "color",
                        choices = colnames(colData(processed_assays()))
      )
    })
    
    observeEvent(input$export, {
      req(processed_assays())
      error_handler(
        add_assays_to_global_rv,
        component_name = "Add assays to global_rv",
        processed_qfeatures = processed_assays(),
        step_number = step_number,
        type = "aggregation",
        varTo = input$fcol,
        varFrom = input$fcol
      )
    })
  })
}
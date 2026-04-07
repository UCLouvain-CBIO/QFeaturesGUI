#' Server for the assay joining tab
#'
#' @param id module id
#' @return The server logic for the assay joining tab
#' @rdname INTERNAL_server_module_join_tab
#' @keywords internal
#'
#' @importFrom shiny moduleServer updateSelectInput observeEvent eventReactive is.reactive
#' @importFrom MultiAssayExperiment getWithColData
#' @importFrom QFeatures joinAssays
#'
server_module_join_tab <- function(id, step_number, step_rv, parent_rv) {
  moduleServer(id, function(input, output, session) {
    pattern <- paste0("_(QFeaturesGUI#", step_number - 1, ")")
    
    step_ready <- reactive({
      if (!is.null(parent_rv)) req(parent_rv() > 0L)
      TRUE
    })
    
    assays_to_process <- reactive({
      req(step_ready())
      error_handler(
        page_assays_subset,
        component_name = "Page assays subset",
        qfeatures = .qf$qfeatures,
        pattern = pattern
      )
    })
    output$joinAvailability <- renderUI({
      n <- length(assays_to_process())
      div(
        style = "text-align : center; padding: 60px 20px;",
        p("Sets will be joined by combining rownames of sets."),
        p(
          paste(n, "sets will be join in 1 set.")
        ),
      )    
    })
    observeEvent(input$export, {
      req(assays_to_process())
      with_task_loader(
        caption = "Be aware that join table can be quite time consuming for large datasets.",
        expr = {
          processed_assays <- error_handler(
            joinAssays,
            component_name = "Join",
            assays_to_process(),
            names(assays_to_process())
          )
          error_handler(
            add_joined_assay_to_global_rv,
            component_name = "Add assays to global_rv",
            processed_qfeatures = processed_assays,
            featuresType = input$feature_type,
            step_number = step_number,
            type = "join"
          )
        }
      )
      step_rv(step_rv() + 1L)
    })
  })
}
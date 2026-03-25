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
      error_handler(
        page_assays_subset,
        component_name = "Page assays subset",
        qfeatures = .qf$qfeatures,
        pattern = pattern
      )
    })
    output$joinAvailability <- renderUI({
      rownamesQF <- rownames(assays_to_process()[[1]])
      for (i in 2:length(assays_to_process())){
        rownamesQF <- c(rownamesQF, rownames(assays_to_process()[[i]]))
        if (length(rownamesQF) != length(unique(rownamesQF))){
          duplicates <- TRUE
          break
        } else {
          duplicates <- FALSE
        }
      }
      n <- length(assays_to_process())
      div(
        style = "text-align : center; padding: 60px 20px;",
        p("Assays will be joined by combining rownames of sets please make sure you use the aggregation step before trying to join sets"),
        p(
          paste(n, "sets will be join in 1 set")
        ),
        if(isTRUE(duplicates)){
          p(
            style = "font-size: 1.1em; color: #777;",
            "Found duplicates in the rownames, the sets cannot be joined",
            tags$i(
              class = "fa fa-xmark"
            )
            
          )
        } else {
          p(
            style = "font-size: 1.1em; color: #777;",
            "No duplicates found in the rownames the sets can be joined",
            tags$i(
              class = "fa fa-check"
            )
          )
        }
      )
      
    })
    observeEvent(input$export, {
      req(assays_to_process())
      shinycssloaders::showPageSpinner(
        type = 6,
        caption = "Be aware that joining can be quite time consuming for large data sets"
      )
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
        step_number = step_number,
        type = "join"
      )
      step_rv(step_rv() + 1L)
      shinycssloaders::hidePageSpinner()
    })
  })
}
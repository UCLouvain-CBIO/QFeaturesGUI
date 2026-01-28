#'
#' @title Workflow Configuration Module Server
#'
#' @param id The module id
#'
#' @return A Shiny module UI function
#' @rdname INTERNAL_server_module_workflow_config
#' @keywords internal
#'
#' @importFrom shiny moduleServer observeEvent
#'
#' @title Workflow Configuration Module Server
#' @param id module id
#' @keywords internal
server_module_workflow_config <- function(id) {
  moduleServer(id, function(input, output, session) {

    session$sendCustomMessage(
      "initWorkflowSortable",
      list(
        palette  = session$ns("palette"),
        workflow = session$ns("workflow"),
        input    = session$ns("workflow_list")
      )
    )

    observeEvent(input$apply, {
      global_rv$workflow_config <- input$workflow_list
    })
  })
}


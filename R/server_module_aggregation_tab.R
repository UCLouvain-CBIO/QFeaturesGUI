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
  moduleServer(id, function(input, output, session))
}
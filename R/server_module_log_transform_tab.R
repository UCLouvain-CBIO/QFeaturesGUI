#' Server for the module log transformation tab
#'
#' @param id module id
#' @return The server logic for the log transformation tab
#' @rdname INTERNAL_server_module_log_transformation_tab
#' @keywords internal
#'
#' @importFrom shiny moduleServer updateSelectInput observeEvent eventReactive is.reactive
#' @importFrom MultiAssayExperiment getWithColData
#'
server_module_log_transform_tab <- function(id, step_number) {
    moduleServer(id, function(input, output, session) {
        return(NULL)
    })
    }
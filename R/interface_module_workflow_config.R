#'
#' @title Workflow Configuration Module (interface)
#' 
#' @param id The module id
#' 
#' @return A Shiny module UI function
#' @rdname INTERNAL_interface_module_workflow_config
#' @keywords internal
#' 
#' @importFrom shiny uiOutput actionButton tagList icon
#' @importFrom htmltools div
#'

interface_module_workflow_config_tab <- function(id) {
    tagList(
        uiOutput(NS(id, "tabs")),
        div(
            actionButton(NS(id, "add"),
                label = "",
                icon = icon("plus"),
                class = "add-button no-bottom-margin",
                width = "80%"
            ),
            class = "container"
        ),
        div(
            actionButton(NS(id, "remove"),
                label = "Remove Last Step",
                class = "suppress-button",
                width = "80%"
            ),
            class = "container"
        ),
        actionButton(NS(id, "apply"),
            label = "Confirm Current Workflow",
            class = "load-button",
            width = "100%"
        ),
    )
}

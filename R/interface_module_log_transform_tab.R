#' log transform tab (section) ui builder
#'
#' @return A shiny tagList object that contains the log transform tab UI components
#' @rdname INTERNAL_interface_module_log_transform_tab
#' @keywords internal
#'
#' @importFrom shiny fluidRow NS actionButton icon uiOutput
#' @importFrom shinydashboardPlus box
#' @importFrom htmltools tagList
#' @importFrom shinyBS bsTooltip
#'
interface_module_log_transform_tab <- function(id) {
    tagList(
        actionButton(
            NS(id, "reload"),
            "Load assays from previous step",
            icon("hand-pointer", class = "fa-solid"),
            width = "100%",
            class = "load-button"
        ),
        shinyBS::bsTooltip(
            id = NS(id, "reload"),
            title = paste("Load the assays from the previous step.",
                "Click on this button the first time you visit this page",
                "or if you updated the assays from the previous steps.",
                sep = " "
            ),
            trigger = "hover"
        ),
    )
}

interface_box_distribution <- function(id) {
    box(
        title = "Log Transformation",
        status = "primary",
        width = 12,
        solidHeader = TRUE,
        collapsible = FALSE,
        
        withSpinner(plotlyOutput(outputId = NS(id, "dist")),
            type = 6,
            color = "#3c8dbc"
        )
    )
}

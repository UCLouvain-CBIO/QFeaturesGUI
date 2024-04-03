#'
#' @title Filtering Box
#'
#' @param id module id
#' @return A shiny module UI function that contains the filtering box
#' @rdname INTERNAL_interface_module_filtering_box
#' @keywords internal
#'
#' @importFrom shiny NS selectInput textInput
#' @importFrom shinydashboardPlus box
#'
interface_module_filtering_box <- function(id) {
    box(
        title = "Filtering Box",
        status = "primary",
        width = 4,
        solidHeader = TRUE,
        collapsible = TRUE,
        interface_module_annotation_plot(NS(id, "annotation_plot")),
        selectInput(
            inputId = NS(id, "annotation_selection"),
            label = "Annotation to Filter",
            choices = NULL
        ),
        selectInput(
            inputId = NS(id, "filter_operator"),
            label = "Filtering Operator",
            choices = c("<", "<=", ">", ">=", "==", "!=")
        ),
        textInput(
            inputId = NS(id, "filter_value"),
            label = "Filtering Value",
            value = "",
            placeholder = "A value that will be used in combinaison with the filter operator."
        )
    )
}

#' @title Annotation Plot
#'
#' @param id module id
#' @return A shiny module UI function that contains the annotation plot
#' @rdname INTERNAL_interface_module_annotation_plot
#' @keywords internal
#'
#' @importFrom shiny NS selectInput actionButton
#' @importFrom shinydashboardPlus box boxSidebar
#' @importFrom plotly renderPlotly
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyBS bsAlert
#'
interface_module_annotation_plot <- function(id) {
    box(
        title = "Annotation Distribution",
        status = "primary",
        width = 12,
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        sidebar = boxSidebar(
            id = NS(id, "sidebar"),
            width = 100,
            selectInput(
                inputId = NS(id, "selected_assay"),
                label = "Select Assay",
                choices = NULL
            )
        ),
        withSpinner(plotlyOutput(NS(id, "plot")),
            type = 6,
            color = "#3c8dbc"
        ),
        bsAlert(NS(id, "alert")) # Not working with NS
    )
}

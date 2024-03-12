#' Title
#'
#' @param id
#'
#' @return a tagList object that contains the UI for the qc metrics module
#' @rdname INTERNAL_interface_module_qc_metrics
#' @keywords internal
#'
#' @importFrom shiny fluidRow tagList selectInput NS
#' @importFrom shinydashboardPlus box
#' @importFrom plotly plotlyOutput
interface_module_pre_qc_metrics <- function(id) {
    tagList(
        fluidRow(
            box(
                title = "PCA",
                status = "primary",
                width = 8,
                solidHeader = FALSE,
                collapsible = TRUE,
                selectInput(
                    inputId = NS(id, "selected_assay"),
                    choices = NULL,
                    label = "Selected assay"
                ),
                plotlyOutput(outputId = NS(id, "pca"))
            ),
            box(
                title = "Feature Annotations",
                status = "primary",
                width = 4,
                solidHeader = FALSE,
                collapsible = TRUE,
                "WIP"
            )
        )
    )
}

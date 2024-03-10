#' Title
#'
#' @param id 
#'
#' @return
#'
#' @importFrom shiny plotOutput fluidRow tagList selectInput NS
#' @importFrom shinydashboardPlus box
interface_module_qc_metrics <- function(id) {
    tagList(
        selectInput(
            inputId = NS(id, "selected_assay"),
            choices = NULL,
            label = "Selected assay"),
        fluidRow(
            box(
                title = "PCA",
                status = "primary",
                width = 8,
                solidHeader = FALSE,
                collapsible = TRUE,
                plotOutput(outputId = NS(id, "pca"))
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

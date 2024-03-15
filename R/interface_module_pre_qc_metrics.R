#' Title
#'
#' @param id
#'
#' @return a tagList object that contains the UI for the qc metrics module
#' @rdname INTERNAL_interface_module_qc_metrics
#' @keywords internal
#'
#' @importFrom shiny fluidRow tagList selectInput NS column
#' @importFrom shinydashboardPlus box boxSidebar
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
                fluidRow(
                    column(
                        6,
                        selectInput(
                            inputId = NS(id, "selected_assay"),
                            choices = NULL,
                            label = "Select assay"
                        )
                    ),
                    column(
                        6,
                        selectInput(
                            inputId = NS(id, "selected_method"),
                            choices = c("nipals"),
                            label = "Select Reduction Method"
                        )
                    )
                ),
                fluidRow(
                    interface_module_pca_box(
                        NS(id, "features"),
                        title = "Features PCA"
                    ),
                    interface_module_pca_box(
                        NS(id, "samples"),
                        title = "Samples PCA"
                    )
                )
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

#'
#' @param id
#' @param title title of the box
#' @return a box object that contains the UI for the pca module
#' @rdname INTERNAL_interface_module_pca_box
#' @keywords internal
#'
#' @importFrom shinydashboardPlus box boxSidebar
#' @importFrom shiny selectInput
#' @importFrom plotly plotlyOutput
#'
interface_module_pca_box <- function(id, title) {
    box(
        title = title,
        status = "primary",
        width = 6,
        solidHeader = FALSE,
        collapsible = TRUE,
        sidebar = boxSidebar(
            id = NS(id, "pca_sidebar"),
            width = 50,
            startOpen = FALSE,
            selectInput(
                inputId = NS(id, "pca_color"),
                label = "Color by",
                choices = NULL
            )
        ),
        plotlyOutput(outputId = NS(id, "pca"))
    )
}

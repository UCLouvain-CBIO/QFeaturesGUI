#' @title Interface for the qc metrics module
#'
#' @param id module id
#'
#' @return a tagList object that contains the UI for the qc metrics module
#' @rdname INTERNAL_interface_module_qc_metrics
#' @keywords internal
#'
#' @importFrom shiny fluidRow tagList selectInput NS column
#' @importFrom shinydashboardPlus box boxSidebar
#' @importFrom plotly plotlyOutput
#'
interface_module_qc_metrics <- function(id, type) {
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
                title = "Single Feature Visualisation",
                status = "primary",
                width = 4,
                solidHeader = FALSE,
                collapsible = TRUE,
                interface_module_viz_box(NS(id, "viz_box"))
            )
        )
    )
}

#' PCA Box interface module
#'
#' @param id module id
#' @param title title of the box
#' @return a box object that contains the UI for the pca module
#' @rdname INTERNAL_interface_module_pca_box
#' @keywords internal
#'
#' @importFrom shinydashboardPlus box boxSidebar
#' @importFrom shiny selectInput checkboxInput numericInput NS
#' @importFrom shinycssloaders withSpinner
#' @importFrom plotly plotlyOutput
#'
interface_module_pca_box <- function(id, title) {
    box(
        title = title,
        status = "primary",
        width = 6,
        solidHeader = TRUE,
        collapsible = FALSE,
        sidebar = boxSidebar(
            id = NS(id, "pca_sidebar"),
            width = 50,
            startOpen = FALSE,
            selectInput(
                inputId = NS(id, "pca_color"),
                label = "Color by",
                choices = NULL
            ),
            checkboxInput(
                inputId = NS(id, "scale"),
                label = "Scale data",
                value = TRUE
            ),
            checkboxInput(
                inputId = NS(id, "center"),
                label = "Center data",
                value = TRUE
            ),
            checkboxInput(
                inputId = NS(id, "show_legend"),
                label = "Show Legend",
                value = FALSE
            ),
            numericInput(
                inputId = NS(id, "color_width"),
                label = "Color value max length (chr)",
                value = 10,
                min = 5,
                max = 30
            )
        ),
        withSpinner(plotlyOutput(outputId = NS(id, "pca")),
            type = 6,
            color = "#3c8dbc"
        )
    )
}

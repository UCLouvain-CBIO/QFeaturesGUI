#' Interface of the viz box module
#' @param id The module id
#' @return The interface of the viz box module
#'
#' @rdname INTERNAL_interface_module_viz_box
#' @keywords internal
#'
#' @importFrom shiny selectizeInput selectInput plotOutput
#' @importFrom plotly plotlyOutput
#'

interface_module_viz_box <- function(id) {
    tagList(
        column(
            4,
            box(
                title = "Parameters",
                status = "primary",
                solidHeader = FALSE,
                collapsible = TRUE,
                collapsed = FALSE,
                width = 12,
                selectInput(NS(id, "feature_type_column"),
                    "Feature Annotation Column",
                    choices = NULL
                ),
                selectInput(NS(id, "sample_type_column"),
                    "Sample Annotation Column",
                    choices = NULL
                ),
                selectizeInput(NS(id, "feature"),
                    "Feature Selection",
                    choices = NULL
                )
            )
        ),
        column(
            8,
            box(
                title = "Box Plot",
                status = "primary",
                solidHeader = FALSE,
                collapsible = TRUE,
                collapsed = FALSE,
                width = 12,
                plotlyOutput(NS(id, "plot"))
            )
        )
    )
}

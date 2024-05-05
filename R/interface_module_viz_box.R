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
        selectizeInput(NS(id, "feature"),
            "Feature Selection",
            choices = NULL
        ),
        selectInput(NS(id, "sample_type_column"),
            "Sample Type Column",
            choices = NULL
        ),
        checkboxInput(NS(id, "scale"),
            "Scale Data",
            value = TRUE
        ),
        box(
            title = "Box Plot",
            status = "primary",
            solidHeader = FALSE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            plotlyOutput(NS(id, "plot"))
        )
    )
}

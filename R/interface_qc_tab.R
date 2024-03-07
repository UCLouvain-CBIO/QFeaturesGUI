#' psm quality control tab (section) ui builder
#'
#' @return A shiny fluidRow object that contains the psm quality control tab UI components
#' @rdname INTERNAL_interface_psm_filtering_tab
#' @keywords internal
#'
#' @importFrom shiny fluidRow
#' @importFrom shinydashboardPlus box
#' @importFrom htmltools tagList h2
#'
psm_filtering_tab <- function() {
    tagList(
        fluidRow(
            box(
                title = "Pre-Filtering Metrics",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE,
                interface_module_qc_metrics()
            )
        ),
        fluidRow(
            box(
                title = "Placeholder Filtering",
                status = "primary",
                width = 4,
                solidHeader = TRUE,
                collapsible = TRUE,
                "WIP"
            ),
            box(
                title = "Placeholder Filtering",
                status = "primary",
                width = 4,
                solidHeader = TRUE,
                collapsible = TRUE,
                "WIP"
            ),
            box(
                title = "Placeholder Filtering",
                status = "primary",
                width = 4,
                solidHeader = TRUE,
                collapsible = TRUE,
                "WIP"
            )
        ),
        fluidRow(
            box(
                title = "Post-Filtering Metrics",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE,
                interface_module_qc_metrics()
            )
        )
    )
}

interface_module_qc_metrics <- function() {
    fluidRow(
        box(
            title = "SCR",
            status = "primary",
            width = 4,
            solidHeader = FALSE,
            collapsible = TRUE,
            "WIP"
        ),
        box(
            title = "PCA",
            status = "primary",
            width = 4,
            solidHeader = FALSE,
            collapsible = TRUE,
            "WIP"
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
}

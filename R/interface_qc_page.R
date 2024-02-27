#' quality control page (section) ui builder
#'
#' @return A shiny fluidRow object that contains the quality control page UI components
#' @rdname INTERNAL_interface_qc_page
#' @keywords internal
#'
#' @importFrom shiny fluidRow
#' @importFrom shinydashboardPlus box
#' @importFrom htmltools h2
#'
qc_page <- function() {
    fluidRow(
        box(
            title = "Quality control",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            h2("Quality control")
        )
    )
}

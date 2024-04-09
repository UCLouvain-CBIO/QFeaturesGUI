#' Summary tab for the interface module
#'
#' @param id the id of the module
#' @return a box with the summary tab
#'
#' @rdname INTERNAL_interface_module_summary_tab
#' @keywords internal
#' @importFrom shinydashboardPlus box
#' @importFrom DT dataTableOutput
#' @importFrom shiny NS
#' @importFrom plotly plotlyOutput
interface_module_summary_tab <- function(id) {
    box(
        title = "QFeatures Summary",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        DT::dataTableOutput(NS(id, "qfeatures_dt")),
        DT::dataTableOutput(NS(id, "assay_table")),
        box(
            title = "Visual Summary",
            status = "primary",
            solidHeader = FALSE,
            collapsible = TRUE,
            width = 12,
            plotlyOutput(NS(id, "qfeatures_plot"))
        )
    )
}

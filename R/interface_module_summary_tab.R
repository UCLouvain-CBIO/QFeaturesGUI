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
#' @importFrom shinycssloaders withSpinner
interface_module_summary_tab <- function(id) {
    box(
        title = "QFeatures Summary",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        withSpinner(DT::dataTableOutput(NS(id, "qfeatures_dt")),
            type = 6,
            color = "#3c8dbc"
        ),
        withSpinner(DT::dataTableOutput(NS(id, "assay_table")),
            type = 6,
            color = "#3c8dbc"
        ),
        box(
            title = "Visual Summary",
            status = "primary",
            solidHeader = FALSE,
            collapsible = TRUE,
            width = 12,
            withSpinner(plotlyOutput(NS(id, "qfeatures_plot")),
                type = 6,
                color = "#3c8dbc"
            )
        ),
        downloadButton(
            outputId = NS(id, "download_qfeatures"),
            "Download QFeatures",
            class = "load-button",
            style = "width: 100%;"
        )
    )
}

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
        waiter::withWaiter(DT::dataTableOutput(NS(id, "qfeatures_dt")),
            html = waiter::spin_fading_circles(),
            color = "rgba(0, 0, 0, 0.25)"
        ),
        waiter::withWaiter(DT::dataTableOutput(NS(id, "assay_table")),
            html = waiter::spin_fading_circles(),
            color = "rgba(0, 0, 0, 0.25)"
        ),
        box(
            title = "Visual Summary",
            status = "primary",
            solidHeader = FALSE,
            collapsible = TRUE,
            width = 12,
            waiter::withWaiter(plotlyOutput(NS(id, "qfeatures_plot")),
                html = waiter::spin_fading_circles(),
                color = "rgba(0, 0, 0, 0.25)"
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

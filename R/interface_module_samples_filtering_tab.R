#' sample quality control tab (section) ui builder
#'
#' @return A shiny fluidRow object that contains the quality control tab UI components (sample QC)
#' @rdname INTERNAL_interface_samples_filtering_tab
#' @keywords internal
#'
#' @importFrom shiny fluidRow NS actionButton icon uiOutput
#' @importFrom shinydashboardPlus box
#' @importFrom htmltools tagList
#' @importFrom shinyBS bsTooltip
#'
interface_module_samples_filtering_tab <- function(id) {
    tagList(
        fluidRow(
            box(
                title = "Pre-Filtering Metrics",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE,
                interface_module_qc_metrics(NS(id, "psm_pre"), "samples")
            )
        ),
        fluidRow(
            box(
                title = "Filtering Boxes Configuration",
                status = "primary",
                width = 4,
                solidHeader = TRUE,
                collapsible = TRUE,
                uiOutput(NS(id, "boxes_summary")),
                style = "min-height: 200px;",
                actionButton(NS(id, "add_box"), "Add Box",
                    width = "100%",
                    class = "load-button no-bottom-margin"
                ),
                actionButton(NS(id, "remove_box"), "Remove Last Box",
                    width = "100%",
                    class = "suppress-button"
                )
            ),
            uiOutput(NS(id, "filtering_boxes")),
            box(
                title = "Filtering Summary",
                status = "primary",
                width = 4,
                solidHeader = TRUE,
                collapsible = TRUE,
                style = "min-height: 200px;",
                uiOutput(NS(id, "filtering_summary")),
                actionButton(NS(id, "apply_filters"),
                    "Apply Filters",
                    width = "100%",
                    class = "load-button"
                ),
                # infoBox("Box_test", 30),
                # infoBox("Percent_test", paste(20,"%"))
                infoBoxOutput(NS(id, "Number_samples_removed")),
                infoBoxOutput(NS(id, "Percent_samples_removed"))
            )
        ),
        fluidRow(
            box(
                title = "Post-Filtering Metrics",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE,
                interface_module_qc_metrics(NS(id, "psm_filtered"), "samples")
            )
        ),
        actionButton(
            NS(id, "export"),
            "Save the processed assays",
            icon("hand-pointer", class = "fa-solid"),
            width = "100%",
            class = "load-button"
        ),
        shinyBS::bsTooltip(
            id = NS(id, "export"),
            title = paste("Write the processed assays to the QFeatures object.",
                "This is needed to proceed to the next steps.",
                sep = " "
            ),
            trigger = "hover",
            placement = "top"
        )
    )
}

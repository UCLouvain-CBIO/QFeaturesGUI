#' features quality control tab (section) ui builder
#'
#' @return A shiny fluidRow object that contains the psm quality control tab UI components
#' @rdname INTERNAL_interface_psm_filtering_tab
#' @keywords internal
#'
#' @importFrom shiny fluidRow NS actionButton icon
#' @importFrom shinydashboardPlus box
#' @importFrom htmltools tagList
#' @importFrom shinyBS bsTooltip
#'
interface_module_features_filtering_tab <- function(id) {
    tagList(
        actionButton(
            NS(id, "reload"),
            "Load assays from previous step",
            icon("hand-pointer", class = "fa-solid"),
            width = "100%",
            class = "load-button"
        ),
        shinyBS::bsTooltip(
            id = NS(id, "reload"),
            title = paste("Load the assays from the previous step.",
                "Click on this button the first time you visit this page",
                "or if you updated the assays from the previous steps.",
                sep = " "
            ),
            trigger = "hover"
        ),
        fluidRow(
            box(
                title = "Pre-Filtering Metrics",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE,
                interface_module_pre_qc_metrics(NS(id, "psm_pre"))
            )
        ),
        fluidRow(
            interface_module_filtering_box(NS(id, "filtering_1")),
            interface_module_filtering_box(NS(id, "filtering_2")),
            interface_module_filtering_box(NS(id, "filtering_3")),
            interface_module_filtering_box(NS(id, "filtering_4")),
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
                interface_module_pre_qc_metrics(NS(id, "psm_filtered"))
            )
        )
    )
}

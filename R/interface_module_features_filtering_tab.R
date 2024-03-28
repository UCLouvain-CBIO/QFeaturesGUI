#' features quality control tab (section) ui builder
#'
#' @return A shiny fluidRow object that contains the psm quality control tab UI components
#' @rdname INTERNAL_interface_psm_filtering_tab
#' @keywords internal
#'
#' @importFrom shiny fluidRow NS actionButton icon uiOutput
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
            box(
                title = "Filtering Boxes Congifuration",
                status = "primary",
                width = 4,
                solidHeader = TRUE,
                collapsible = TRUE,
                numericInput(NS(id, "n_boxes"), "Number of filtering boxes", value = 3, min = 1, max = 10),
                actionButton(NS(id, "generate_boxes"), "Generate Boxes")
            ),
            uiOutput(NS(id, "filtering_boxes"))
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

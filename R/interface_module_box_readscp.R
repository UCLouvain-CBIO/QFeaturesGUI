#' A UI module that return a box that contains the UI components
#'  to create an preview a QFeatures object
#'
#' @param id module id
#'
#' @return A shinydashboardPlus box object that contains the UI components to create an preview a QFeatures object
#' @rdname INTERNAL_box_readscp_ui
#' @keywords internal
#'
#' @importFrom shiny tagList selectInput checkboxInput actionButton downloadButton NS
#' @importFrom shinydashboardPlus box
#' @importFrom DT dataTableOutput
#'
box_readscp_ui <- function(id) {
    tagList(
        box(
            title = "QFeatures Converter",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            box(
                title = "Parameters",
                status = "primary",
                width = 12,
                solidHeader = FALSE,
                collapsible = TRUE,
                id = NS(id, "parameters"),
                selectInput(
                    inputId = NS(id, "batch_col"),
                    "Batch column :",
                    choices = NULL
                ),
                selectInput(
                    inputId = NS(id, "channel_col"),
                    "Channel column :",
                    choices = NULL
                ),
                checkboxInput(
                    inputId = NS(id, "removeEmptyCols"),
                    label = "Remove columns that contain only missing values",
                    value = FALSE
                ),
                checkboxInput(
                    inputId = NS(id, "zero_as_NA"),
                    label = "Convert zeros to NA",
                    value = TRUE
                ),
                actionButton(
                    inputId = NS(id, "convert"),
                    "Convert to a QFeatures object"
                ),
                downloadButton(
                    outputId = NS(id, "download_qfeatures"),
                    "Download"
                )
            ),
            box(
                title = "QFeatures Preview",
                status = "primary",
                width = 12,
                solidHeader = FALSE,
                collapsible = TRUE,
                id = NS(id, "qfeatures_preview"),
                DT::dataTableOutput(NS(id, "qfeatures_dt"))
            ),
            box(
                title = "Selected Assay Preview",
                status = "primary",
                width = 12,
                solidHeader = FALSE,
                collapsible = TRUE,
                id = NS(id, "assay_preview"),
                DT::dataTableOutput(NS(id, "assay_table"))
            )
        )
    )
}

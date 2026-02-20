#' A UI module that return a box that contains the UI components
#'  to create an preview a QFeatures object
#'
#' @param id module id
#'
#' @return A shinydashboardPlus box object that contains the UI components to create an preview a QFeatures object
#' @rdname INTERNAL_box_readqfeatures_ui
#' @keywords internal
#'
#' @importFrom shiny tagList selectInput checkboxInput actionButton downloadButton NS
#' @importFrom shinydashboardPlus box
#' @importFrom DT dataTableOutput
#' @importFrom shinyjs disabled hidden
#'
box_readqfeatures_ui <- function(id) {
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
                    inputId = NS(id, "run_col"),
                    "Run/Batch column :",
                    choices = NULL,
                    selected = "NULL"
                ),
                selectInput(
                    inputId = NS(id, "quant_cols"),
                    "Quantitative column : (Only relevant without sample table)",
                    choices = NULL,
                    multiple = TRUE
                ),
                checkboxInput(
                    inputId = NS(id, "removeEmptyCols"),
                    label = "Remove columns that contain only missing values",
                    value = FALSE
                ),
                checkboxInput(
                    inputId = NS(id, "logTransform"),
                    label = "Log transform data",
                    value = TRUE
                ),
                checkboxInput(
                    inputId = NS(id, "zero_as_NA"),
                    label = "Convert zeros to NA",
                    value = TRUE
                ),
                checkboxInput(
                    inputId = NS(id, "singlecell"),
                    label = "Single cell data",
                    value = FALSE
                ),
                disabled(
                    actionButton(
                        inputId = NS(id, "convert"),
                        "Convert to a QFeatures object",
                        class = "add-button no-bottom-margin",
                        width = "100%"
                    )
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
            ),
            hidden(
                downloadButton(
                    outputId = NS(id, "downloadQFeatures"),
                    "Download QFeatures object",
                    class = "load-button",
                    style = "width: 100%;"
                )
            )
        )
    )
}

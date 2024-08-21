#' A UI module that return a box that contains the UI components to read a table
#'
#' @param id module id
#'
#' @return A shiny box object that contains the UI components to read a table
#' @rdname INTERNAL_box_read_table_ui
#' @keywords internal
#'
#' @importFrom shiny tagList fileInput fluidRow NS textInput numericInput checkboxInput actionButton
#' @importFrom shinydashboardPlus box
#' @importFrom DT dataTableOutput
#'
box_read_table_ui <- function(id) {
    tagList(
        box(
            title = if (id == "input") "assayData" else "colData",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            fileInput(
                inputId = NS(id, "file"),
                label = NULL,
                accept = c(".csv", ".tsv")
            ),
            fluidRow(
                box(
                    title = "Preview",
                    status = "primary",
                    width = 8,
                    solidHeader = FALSE,
                    collapsible = TRUE,
                    DT::dataTableOutput(NS(id, "dt_table"))
                ),
                box(
                    title = "Parameters",
                    status = "primary",
                    width = 4,
                    solidHeader = FALSE,
                    collapsible = TRUE,
                    id = NS(id, "parameters"),
                    textInput(
                        inputId = NS(id, "sep"),
                        label = "Separator character",
                        value = ","
                    ),
                    textInput(
                        inputId = NS(id, "dec"),
                        label = "Decimal character",
                        value = "."
                    ),
                    textInput(
                        inputId = NS(id, "comment_char"),
                        label = "Comment character",
                        value = "#"
                    ),
                    numericInput(
                        inputId = NS(id, "skip"),
                        label = "Number of line to skip before reading data",
                        value = 0
                    ),
                    checkboxInput(
                        inputId = NS(id, "stringsAsFactors"),
                        label = "String as Factor",
                        value = FALSE
                    ),
                    actionButton(
                        inputId = NS(id, "import_button"),
                        label = "Import",
                        width = "100%",
                        style = "color: #fff; background-color: #3c8dbc; border-color: #2e6da4"
                    )
                )
            )
        )
    )
}

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
                actionButton(
                    inputId = NS(id, "convert"),
                    "Convert to a QFeatures object"
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

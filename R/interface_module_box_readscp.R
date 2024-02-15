box_readscp_ui <- function(id) {
    tagList(
        box(
            title = "Converting",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            fluidRow(
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
                        label = "Removing columns that contain only missing values",
                        value = FALSE
                    ),
                    actionButton(
                        inputId = NS(id, "convert"),
                        "Convert to a QFeatures object"
                    )
                )
            )
        )
    )
}

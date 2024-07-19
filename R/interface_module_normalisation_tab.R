#' normalisation tab (section) ui builder
#'
#' @return A shiny tagList object that contains the normalisation tab UI components
#' @rdname INTERNAL_interface_module_normalisation_tab
#' @keywords internal
#'
#' @importFrom shiny fluidRow NS actionButton icon uiOutput
#' @importFrom shinydashboardPlus box
#' @importFrom htmltools tagList h2
#' @importFrom shinyBS bsTooltip
#'
interface_module_normalisation_tab <- function(id) {
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
        box(
            title = "Normalisation",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsible = FALSE,
            fluidRow(
                box(
                    title = "",
                    status = "primary",
                    width = 9,
                    solidHeader = FALSE,
                    collapsible = TRUE,
                    withSpinner(plotlyOutput(outputId = NS(id, "density_plot")),
                        type = 6,
                        color = "#3c8dbc"
                    )
                ),
                box(
                    title = "Settings",
                    status = "primary",
                    width = 3,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    selectInput(
                        inputId = NS(id, "method"),
                        label = "method",
                        choices = c(
                            "sum",
                            "max",
                            "center.mean",
                            "center.median",
                            "div.mean",
                            "div.median",
                            "diff.meda",
                            "quantiles",
                            "quantiles.robust",
                            "vsn"
                        ),
                        selected = "center.median"
                    ),
                    br(),
                    h4("Plot options"),
                    selectInput(
                        inputId = NS(id, "color"),
                        label = "Color by",
                        choices = NULL
                    )
                )
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

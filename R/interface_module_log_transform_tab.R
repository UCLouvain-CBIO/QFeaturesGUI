#' log transform tab (section) ui builder
#'
#' @return A shiny tagList object that contains the log transform tab UI components
#' @rdname INTERNAL_interface_module_log_transform_tab
#' @keywords internal
#'
#' @importFrom shiny fluidRow NS actionButton icon uiOutput
#' @importFrom shinydashboardPlus box
#' @importFrom htmltools tagList
#' @importFrom shinyBS bsTooltip
#'
interface_module_log_transform_tab <- function(id) {
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
        interface_box_distribution("box_distribution"),
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

interface_box_distribution <- function(id) {
    box(
        title = "Log Transformation",
        status = "primary",
        width = 12,
        solidHeader = TRUE,
        collapsible = FALSE,
        fluidRow(
            box(
                title = "Prior Distribution",
                status = "primary",
                width = 5,
                solidHeader = TRUE,
                collapsible = TRUE,
                withSpinner(plotlyOutput(outputId = NS(id, "prior_dist")),
                    type = 6,
                    color = "#3c8dbc"
                )
            ),
            box(
                title = "Post Distribution",
                status = "primary",
                width = 5,
                solidHeader = TRUE,
                collapsible = TRUE,
                withSpinner(plotlyOutput(outputId = NS(id, "post_dist")),
                    type = 6,
                    color = "#3c8dbc"
                )
            ),
            box(
                title = "Transformation Settings",
                status = "primary",
                width = 2,
                solidHeader = TRUE,
                collapsible = TRUE,
                selectInput(inputId = NS(id, "log_base"),
                    label = "Log Base",
                    choices = c(2, 10),
                    selected = 2
                )
            )
        )
    )
}

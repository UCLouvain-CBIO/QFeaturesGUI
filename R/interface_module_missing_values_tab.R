#' QFeatures missing values tab (section) ui builder
#'
#' @return A shiny fluidRow object that contains the missing values tab UI components
#' @rdname INTERNAL_interface_module_missing_values_tab
#' @keywords internal
#'
#' @importFrom shiny fluidRow NS actionButton icon uiOutput checkboxInput selectInput numericInput
#' @importFrom shinydashboardPlus box boxSidebar
#' @importFrom shinydashboard infoBoxOutput
#' @importFrom htmltools tagList
#' @importFrom shinyBS bsTooltip
#'
interface_module_missing_values_tab <- function(id, type) {
    tagList(
        box(
          numericInput(
            inputId = NS(id, paste0("threshold_", type)),
            label = paste("Threshold value for", type),
            value = 0.95,
            min = 0, max = 1,
            step = 0.05
          ),
          infoBoxOutput(
            NS(id, paste0("nb_removed_", type)),
            width = 6
          ),
          infoBoxOutput(
            NS(id, paste0("percent_removed_", type)),
            width = 6
          )
        ),
        box(
            title = paste("NA by", type),
            sidebar = boxSidebar(
                id = "plot_sidebar",
                width = 50,
                startOpen = FALSE,
                selectInput(
                    inputId = NS(id, paste0("pca_color_", type)),
                    label = "Color by",
                    choices = NULL
                ),
                checkboxInput(
                    inputId = NS(id, paste0("show_legend_", type)),
                    label = "Show Legend",
                    value = FALSE
                )
            ),
            uiOutput(NS(id, paste0("dynamic_", type)))
        ),
        actionButton(
            NS(id, "export"),
            "Save the processed sets",
            icon("hand-pointer", class = "fa-solid"),
            width = "100%",
            class = "load-button"
        ),
        shinyBS::bsTooltip(
            id = NS(id, "export"),
            title = paste("Write the processed sets to the QFeatures object.",
                "This is needed to proceed to the next steps.",
                sep = " "
            ),
            trigger = "hover",
            placement = "top"
        )
    )
}

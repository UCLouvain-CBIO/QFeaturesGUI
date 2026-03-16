#' aggregation tab (section) ui builder
#'
#' @return A shiny tagList object that contains the aggregation tab UI components
#' @rdname INTERNAL_interface_module_aggregation_tab
#' @keywords internal
#'
#' @importFrom shiny fluidRow NS actionButton icon uiOutput
#' @importFrom shinydashboardPlus box
#' @importFrom htmltools tagList h2
#' @importFrom shinyBS bsTooltip
#'
interface_module_aggregation_tab <- function(id) {
  tagList(
    box(
      title = "Aggregation",
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
            label = "function to aggregate",
            choices = c(
              "robustSummary",
              "medianPolish",
              "colMeans",
              "colMedians",
              "colSums"
            ),
            selected = "medianPolish"
          ),
          br(),
          selectInput(
            inputId = NS(id, "fcol"),
            "rowData variable defining the features of the assay to aggregate",
            choices = NULL,
            multiple = TRUE
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
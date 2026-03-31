#' aggregation tab (section) ui builder
#'
#' @return A shiny tagList object that contains the aggregation tab UI components
#' @rdname INTERNAL_interface_module_aggregation_tab
#' @keywords internal
#'
#' @importFrom shiny fluidRow NS actionButton icon uiOutput textOutput
#' @importFrom shinydashboardPlus box
#' @importFrom htmltools tagList h2
#' @importFrom shinyBS bsTooltip
#'
interface_module_aggregation_tab <- function(id) {
  tagList(
    fluidRow(
      box(
        title = "Aggregation boxplot",
        status = "primary",
        width = 9,
        solidHeader = TRUE,
        collapsible = FALSE,
        div(
          style = "text-align: center; font-size: 16px; color: #777;",
          textOutput(NS(id,"pre_boxplot"))
        ),
        uiOutput(NS(id, "aggregation_boxplot_ui"))
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
          choices = NULL
        ),
        br(),
        selectizeInput(
          inputId = NS(id, "features"),
          "Features to plot",
          choices =  NULL
        ),
        br(),
        tags$h4("Plot options"),
        selectInput(
          inputId = NS(id, "color"),
          label = "Color by",
          choices = NULL
        ),
        checkboxInput(
          inputId = NS(id, "addPoints"),
          label = "Show points",
          value = TRUE
        ),
        actionButton(
          inputId = NS(id, "aggregate"),
          label = "Aggregate",
          width = "100%",
          class = "load-button" 
        )
      )
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

#' boxplot box (section) ui builder
#'
#' @return A shiny tagList object that contains the boxplot box UI components
#' @rdname INTERNAL_interface_module_feature_levels_boxplot
#' @keywords internal
#'
#' @importFrom shiny fluidRow NS actionButton icon uiOutput
#' @importFrom shinydashboardPlus box
#'

interface_module_feature_levels_boxplot <- function(id) {
    waiter::withWaiter(
        plotlyOutput(outputId = NS(id, "boxplot")),
        html = waiter::spin_fading_circles(),
        color = "rgba(0, 0, 0, 0.25)"
    )
}

#' Assay joining tab (section) ui builder
#'
#' @return A shiny tagList object that contains the join tab UI components
#' @rdname INTERNAL_interface_module_join_tab
#' @keywords internal
#'
#' @importFrom shiny fluidRow NS actionButton icon uiOutput
#' @importFrom shinydashboardPlus box
#' @importFrom htmltools tagList h2
#' @importFrom shinyBS bsTooltip
#'
interface_module_join_tab <- function(id) {
  tagList(
    box(
      title = "Join assays",
      status = "primary",
      width = 12,
      solidHeader = TRUE,
      collapsible = FALSE,
      p(
        "Assays will be joined by combining common features ",
        "across assays. Please specify how you want to define ",
        "the feature identifier. This can either be based on ",
        "the assay rownames or on an available feature ",
        "annotation."
      ),
      selectInput(
        inputId = NS(id, "fcol_join"),
        label = "Annotation name",
        choices = NULL
      ),
      p(
        "You can provide a second annotation variable that ",
        "will be pasted to the first annotation to generate ",
        "the feature identifier."
      ),
      selectInput(
        inputId = NS(id, "fcol2_join"),
        label = "Second annotation name (optional)",
        choices = NULL
      ),
      actionButton(
        inputId = NS(id, "join"),
        label = "Join assays",
        class = "load-button"
      ),
      uiOutput(NS(id, "rownames"))
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
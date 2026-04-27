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
    uiOutput(
      NS(id, "joinAvailability")
    ),
    div(
      style = "display: flex; justify-content: center;  text-align : center;",
      shinyFeedback::useShinyFeedback(),
      textInput(
        NS(id, "feature_type"),
        label = "Choose the type of features your set contain."
      ),
      textOutput(
        NS(id, "output")
      )
    ),
    actionButton(
      NS(id, "export"),
      "Join and save the processed sets",
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
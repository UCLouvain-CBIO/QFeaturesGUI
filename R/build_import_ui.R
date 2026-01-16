#' UI builder
#'
#' @return A shiny dashboard UI for importQFeatures function
#' @rdname INTERNAL_build_import_ui
#' @keywords internal
#'
#' @importFrom shinydashboard dashboardBody tabItem tabItems
#' @importFrom shinydashboardPlus dashboardPage
#' @importFrom htmltools includeCSS
#' @importFrom shinyFeedback useShinyFeedback
#'
build_import_ui <- function() {
  ui <- dashboardPage(
    skin = "blue",
    header = header("importQFeatures"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      import_tab()
    )
  )
  return(ui)
}

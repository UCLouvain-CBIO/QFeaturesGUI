#' ui header builder
#'
#' @return the dashboardHeader object for the QFeaturesImport app.
#' @rdname INTERNAL_interface_header
#' @keywords internal
#'
#' @importFrom shinydashboard dropdownMenuOutput
#' @importFrom shinydashboardPlus dashboardHeader
#'
header <- function() {
    dashboardHeader(
        title = "QFeaturesImport",
        dropdownMenuOutput("exception_menu")
    )
}

#' ui header builder
#'
#' @return the dashboardHeader object for the importQFeatures app.
#' @rdname INTERNAL_interface_header
#' @keywords internal
#'
#' @importFrom shinydashboard dropdownMenuOutput
#' @importFrom shinydashboardPlus dashboardHeader
#'
header <- function() {
    dashboardHeader(
        title = "importQFeatures",
        dropdownMenuOutput("exception_menu")
    )
}

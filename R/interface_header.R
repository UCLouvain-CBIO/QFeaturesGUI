#' ui header builder
#'
#' @return the dashboardHeader object for the scpGUI app.
#' @rdname INTERNAL_interface_header
#' @keywords internal
#'
#' @importFrom shinydashboard dropdownMenuOutput
#' @importFrom shinydashboardPlus dashboardHeader
#'
header <- function() {
    dashboardHeader(
        title = "scpGUI",
        dropdownMenuOutput("exception_menu")
    )
}

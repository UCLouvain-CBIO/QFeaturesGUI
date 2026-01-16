#' ui header builder
#'
#' @param title a string that refers to the title of the app 
#'
#' @return the dashboardHeader object for the importQFeatures app
#' @rdname INTERNAL_interface_header
#' @keywords internal
#'
#' @importFrom shinydashboard dropdownMenuOutput
#' @importFrom shinydashboardPlus dashboardHeader
#'
header <- function(title) {
    dashboardHeader(
        title = title,
        dropdownMenuOutput("exception_menu")
    )
}

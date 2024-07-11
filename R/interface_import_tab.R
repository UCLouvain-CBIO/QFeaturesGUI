#' import tab (section) ui builder
#'
#' @return A shiny fluidRow object that contains the import page UI components (3 boxes)
#' @rdname INTERNAL_interface_import_tab
#' @keywords internal
#'
#' @importFrom shiny fluidRow column
import_tab <- function() {
    fluidRow(
        column(
            width = 6,
            box_read_table_ui("input"),
            box_read_table_ui("sample")
        ),
        column(
            width = 6,
            box_readqfeatures_ui("readqfeatures")
        )
    )
}

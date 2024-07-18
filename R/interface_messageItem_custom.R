# This function is a copy of the messageItem function from the shinydashboardPlus package
# This custom definition serve two goals:
# 1. It change the icon used in for the clock that was no more available from font-awesome
# 2. It remove the icon from the messageItem (and margin removed)
#' A custom version of the messageItem function from the shinydashboardPlus package
#'
#' @param inputId If not NULL, this item behaves like an action button.
#' @param class css class
#' @param from Who the message is from.
#' @param message Text of the message.
#' @param icon An icon tag, created by \code{\link[shiny]{icon}}.
#' @param time String representing the time the message was sent. Any string may
#'   be used. For example, it could be a relative date/time like "5 minutes",
#'   "today", or "12:30pm yesterday", or an absolute time, like "2014-12-01 13:45".
#'   If NULL, no time will be displayed.
#' @param href An optional URL to link to.
#'
#' @return a messageItem component that can be used in a dropdownMenu or a messageItemBox.
#' @rdname INTERNAL_interface_messageItem_custom
#' @keywords internal
#'
#' @importFrom shiny icon
#' @importFrom htmltools a h4 p tags
#'
messageItem <- function(from, message, icon = shiny::icon("user"), time = NULL,
    href = NULL, inputId = NULL, class = NULL) {
    if (is.null(href)) href <- "#"
    htmltools::tags$li(
        a(
            id = inputId,
            class = paste0(
                "custom-message ",
                if (!is.null(inputId)) "action-button"
            ),
            href = href,
            h4(class = "no-margin", from, if (!is.null(time)) {
                htmltools::tags$small(shiny::icon("clock"), time)
            }), p(class = "no-margin", message)
        )
    )
}

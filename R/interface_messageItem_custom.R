# This function is a copy of the messageItem function from the shinydashboardPlus package
# This custom definition serve two goals:
# 1. It change the icon used in for the clock that was no more available from font-awesome
# 2. It remove the icon from the messageItem (and margin removed)
messageItem <- function(from, message, icon = shiny::icon("user"), time = NULL,
                        href = NULL, inputId = NULL, class = NULL) {
    if (is.null(href)) href <- "#"
    shiny::tags$li(
        shiny::a(
            id = inputId,
            class = paste0("custom-message ", if (!is.null(inputId)) "action-button"),
            href = href,
            shiny::h4(class = "no-margin", from, if (!is.null(time)) {
                shiny::tags$small(shiny::icon("clock"), time)
            }), shiny::p(class = "no-margin", message)
        )
    )
}

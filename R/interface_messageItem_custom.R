messageItem <- function(from, message, icon = shiny::icon("user"), time = NULL,
                        href = NULL, inputId = NULL) {
    shinydashboardPlus:::tagAssert(icon, type = "i")
    if (is.null(href)) href <- "#"
    shiny::tags$li(
        shiny::a(
            id = inputId,
            class = if (!is.null(inputId)) "action-button",
            href = href,
            icon,
            shiny::h4(from, if (!is.null(time)) {
                shiny::tags$small(shiny::icon("clock"), time)
            }), shiny::p(message)
        )
    )
}

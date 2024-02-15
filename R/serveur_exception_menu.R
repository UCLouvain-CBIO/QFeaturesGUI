server_exception_menu <- function(input, output, session) {
    msgs <- reactive({
        if (nrow(global_rv$exception_data) == 0) {
            return(list())
        }
        apply(global_rv$exception_data, 1, function(row) {
            messageItem(
                from = row[["type"]],
                message = row[["message"]],
                icon = icon("exclamation"),
                time = row[["time"]]
            )
        })
    })
    output$exception_menu <- renderMenu({
        dropdownMenu(
            type = "messages",
            icon = icon("warning"),
            badgeStatus = "danger",
            .list = msgs()
        )
    })
}

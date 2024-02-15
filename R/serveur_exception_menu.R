server_exception_menu <- function(input, output, session, exception_data) {
    stopifnot(is.reactive(exception_data))

    msgs <- reactive({
        if (nrow(exception_data()) == 0) {
            return(list())
        }
        apply(exception_data(), 1, function(row) {
            print("test")
            messageItem(from = row[["type"]], message = row[["message"]])
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
    

    return(exception_data)
}

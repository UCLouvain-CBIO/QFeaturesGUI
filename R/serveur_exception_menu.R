server_exception_menu <- function(input, output, session) {
    msgs <- reactive({
        if (nrow(global_rv$exception_data) == 0) {
            return(list())
        }
        lapply(seq_len(nrow(global_rv$exception_data)), function(i) {
            row <- global_rv$exception_data[i, ]
            id <- paste0("exception_", as.character(i))
            messageItem(
                from = paste0(row[["type"]], " #", i),
                message = row[["message"]],
                icon = icon("exclamation"),
                time = format(
                    row[["time"]],
                    "%m/%d/%Y %H:%M:%S"
                ),
                inputId = id
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

    observe({
        for (i in seq_len(nrow(global_rv$exception_data))) {
            id <- paste0("exception_", i)
            observeEvent(input[[id]],
                {
                    showModal(modalDialog(
                        title = paste0(
                            global_rv$exception_data[i, "type"],
                            " #",
                            i
                        ),
                        global_rv$exception_data[i, "message"],
                        div(
                            class = "error-text",
                            verbatimTextOutput(paste0("message_", i))
                        ),
                        easyClose = TRUE,
                        size = "l"
                    ))
                    output[[paste0("message_", i)]] <- renderText({
                        global_rv$exception_data[i, "full_message"]
                    })
                },
                ignoreNULL = TRUE
            )
        }
    })
}

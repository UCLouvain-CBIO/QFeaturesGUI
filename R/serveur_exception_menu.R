#' The server logic behind the ecxeption dropdown menu
#'
#' @param input input parameter that should be given by
#'  the higher level server builder
#' @param output output parameter that should be given by
#'  the higher level server builder
#' @param session session parameter that should be given by
#'  the higher level server builder
#'
#' @return NULL
#' @rdname INTERNAL_server_exception_menu
#' @keywords internal
#'
#' @importFrom shiny reactive observe observeEvent showModal modalDialog modalButton verbatimTextOutput renderText
#' @importFrom shinydashboard dropdownMenu renderMenu
#' @importFrom htmltools div h3 HTML span br
server_exception_menu <- function(input, output, session) {
    msgs <- reactive({
        if (nrow(global_rv$exception_data) == 0) {
            return(list())
        }
        lapply(seq_len(nrow(global_rv$exception_data)), function(i) {
            row <- global_rv$exception_data[i, ]
            id <- paste0("exception_", as.character(i))
            messageItem(
                from = HTML("<br>"),
                message = HTML(paste0(
                    "<b>", row[["title"]], "</b>", br(),
                    span(HTML("<i>click for more details</i>"),
                        class = "right-align"
                    )
                )),
                icon = icon("exclamation"),
                time = format(
                    row[["time"]],
                    "%H:%M:%S"
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
                        title = global_rv$exception_data[i, "title"],
                        div(
                            class = "italic-text",
                            "This error occurred at ",
                            format(
                                global_rv$exception_data[i, "time"],
                                "%H:%M:%S"
                            )
                        ),
                        h3("Function call:"),
                        div(
                            class = "normal-text",
                            verbatimTextOutput(paste0("func_call_", i))
                        ),
                        h3(paste0(
                            "Full ",
                            global_rv$exception_data[i, "type"],
                            " message:"
                        )),
                        div(
                            class = "error-text",
                            verbatimTextOutput(paste0("message_", i))
                        ),
                        div(
                            class = "italic-text right-align",
                            HTML(paste0(
                                "Note that this error message comes from a function that is not part of this shiny package.",
                                br(),
                                "Please refer to the adequate documentation for more information about the cause of the error."
                            ))
                        ),
                        easyClose = TRUE,
                        size = "l",
                        footer = modalButton("Close")
                    ))
                    output[[paste0("func_call_", i)]] <- renderText({
                        gsub(
                            ",",
                            ",\n    ",
                            global_rv$exception_data[i, "func_call"]
                        )
                    })
                    output[[paste0("message_", i)]] <- renderText({
                        global_rv$exception_data[i, "full_message"]
                    })
                },
                ignoreNULL = TRUE
            )
        }
    })
}

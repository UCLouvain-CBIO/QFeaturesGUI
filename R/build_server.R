.build_server <- function() {
    server <- function(input, output, session) {
        global_rv$exception_data <- data.frame(
            title = character(),
            type = character(),
            func_call = character(),
            message = character(),
            full_message = character(),
            time = as.POSIXct(character())
        )
        server_exception_menu(input, output, session)
        server_import_page(input, output, session)
    }

    server
}

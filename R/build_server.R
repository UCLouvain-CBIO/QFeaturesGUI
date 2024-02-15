.build_server <- function() {
    server <- function(input, output, session) {
        exception_data <- reactiveVal(
            data.frame(
                type = character(),
                message = character(),
                full_message = character()
            )
        )
        server_exception_menu(input, output, session, exception_data)
        server_import_page(input, output, session, exception_data)
    }

    server
}

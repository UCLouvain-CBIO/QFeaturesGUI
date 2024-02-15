.build_server <- function() {
    server <- function(input, output, session) {
        server_exception_menu(input, output, session)
        server_import_page(input, output, session)
    }

    server
}

.build_server <- function() {
    server <- function(input, output, session) {
        server_import_page(input, output, session)
    }

    return(server)
}

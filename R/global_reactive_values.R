global_rv <- reactiveValues(
    "exception_data" = data.frame(
        type = character(),
        message = character(),
        full_message = character(),
        time = as.POSIXct(character())
    )
)

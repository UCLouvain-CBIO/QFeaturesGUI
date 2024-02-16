global_rv <- reactiveValues(
    "exception_data" = data.frame(
        title = character(),
        type = character(),
        func_call = character(),
        message = character(),
        full_message = character(),
        time = as.POSIXct(character())
    )
)

error_handler <- function(func, ...) {
    tryCatch(
        {
            func(...)
        },
        warning = function(w) {
            showNotification("Caught a warning: ",
                conditionMessage(w),
                duration = NULL,
                type = "warning"
            )
            add_exception(
                type = "warning",
                message = conditionMessage(w),
                full_message = w,
                time = Sys.time()
            )
            return(NULL)
        },
        error = function(e) {
            showNotification("Caught an error: ",
                conditionMessage(e),
                duration = NULL,
                type = "error"
            )
            add_exception(
                type = "error",
                message = conditionMessage(e),
                full_message = e,
                time = Sys.time()
            )
            return(NULL)
        }
    )
}

add_exception <- function(type, message, full_message, time) {
    new_data <- data.frame(
        type = as.character(type),
        message = as.character(message),
        full_message = as.character(full_message),
        time = as.POSIXct(time),
        stringsAsFactors = FALSE
    )
    old_data <- global_rv$exception_data
    global_rv$exception_data <- rbind(new_data, old_data)
}

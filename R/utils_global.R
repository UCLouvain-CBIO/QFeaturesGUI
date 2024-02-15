error_handler <- function(func, exception_data, ...) {
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
                exception_data = exception_data
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
                exception_data = exception_data
            )
            return(NULL)
        }
    )
}

add_exception <- function(type, message, full_message, exception_data) {
    new_data <- data.frame(
        type = as.character(type),
        message = as.character(message),
        full_message = as.character(full_message),
        stringsAsFactors = FALSE
    )
    old_data <- exception_data()
    exception_data(rbind(old_data, new_data))
}

error_handler <- function(func, component_name, ...) {
    tryCatch(
        {
            func(...)
        },
        warning = function(w) {
            time <- Sys.time()
            showNotification(
                HTML(
                    paste0(
                        div(HTML(
                            paste0(
                                "<b> Error in ",
                                component_name,
                                " </b> at ", format(time, "%H:%M:%S")
                            )
                        )),
                        div(HTML(
                            "<i>Check the top right exception dropdown menu for more details</i>" # nolint
                        ))
                    )
                ),
                duration = 60,
                type = "warning"
            )
            add_exception(
                title = paste0("Warning in ", component_name),
                type = "warning",
                message = conditionMessage(w),
                full_message = w,
                time = time
            )
            return(NULL)
        },
        error = function(e) {
            time <- Sys.time()
            showNotification(
                HTML(
                    paste0(
                        div(HTML(
                            paste0(
                                "<b> Error in ",
                                component_name,
                                " </b> at ", format(time, "%H:%M:%S")
                            )
                        )),
                        div(HTML(
                            "<i>Check the top right exception dropdown menu for more details</i>" # nolint
                        ))
                    )
                ),
                duration = 60,
                type = "error"
            )
            add_exception(
                title = paste0("Error in ", component_name),
                type = "error",
                message = conditionMessage(e),
                full_message = e,
                time = time
            )
            return(NULL)
        }
    )
}

add_exception <- function(title, type, message, full_message, time) {
    new_data <- data.frame(
        title = as.character(title),
        type = as.character(type),
        message = as.character(message),
        full_message = as.character(full_message),
        time = as.POSIXct(time),
        stringsAsFactors = FALSE
    )
    old_data <- global_rv$exception_data
    global_rv$exception_data <- rbind(new_data, old_data)
}

upper_first <- function(string) {
    substr(string, 1, 1) <- toupper(substr(string, 1, 1))
    return(string)
}

#' An wrapper function to handle errors and warnings
#' Will create a notification and add the exception to the global exception data
#' @param func function that is wrapped
#' @param component_name `str` name of the component (will be reported in the exception message)
#' @param ... arguments to be passed to the function
#'
#' @return NULL
#' @rdname INTERNAL_error_handler
#' @keywords internal
#'
#' @importFrom shiny showNotification
#' @importFrom htmltools HTML div
error_handler <- function(func, component_name, ...) {
    tryCatch(
        {
            func_call <- gsub(
                "\\s+", " ",
                paste(deparse(substitute(func(...))), collapse = " ")
            )
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
                duration = 30,
                type = "warning"
            )
            add_exception(
                title = paste0("Warning in ", component_name),
                type = "warning",
                func_call = func_call,
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
                duration = 30,
                type = "error"
            )
            add_exception(
                title = paste0("Error in ", component_name),
                type = "error",
                func_call = func_call,
                message = conditionMessage(e),
                full_message = e,
                time = time
            )
            return(NULL)
        }
    )
}

#' A function that will add an exception entry to the global exception data
#'
#' @param title `str` title of the exception
#' @param type `str` type of the exception c("warning", "error")
#' @param func_call `str` function call that caused the exception
#' @param message `str` message of the exception
#' @param full_message `str` full message of the exception
#' @param time `POSIXct` time of the exception
#'
#' @return NULL
#' @rdname INTERNAL_add_exception
#' @keywords internal
#'
add_exception <- function(title, type, func_call, message, full_message, time) {
    new_data <- data.frame(
        title = as.character(title),
        type = as.character(type),
        func_call = as.character(func_call),
        message = as.character(message),
        full_message = as.character(full_message),
        time = as.POSIXct(time),
        stringsAsFactors = FALSE
    )
    old_data <- global_rv$exception_data
    global_rv$exception_data <- rbind(new_data, old_data)
}

#' A little function that will capitalize the first letter of a string
#'
#' @param string `str` string to capitalize the first letter
#'
#' @return `str` the string with the first letter capitalized
#' @rdname INTERNAL_upper_first
#' @keywords internal
#'
upper_first <- function(string) {
    substr(string, 1, 1) <- toupper(substr(string, 1, 1))
    return(string)
}

#' A function that will create a loading modal component
#'
#' @param msg `str` message to display in the loading modal
#'
#' @return NULL
#' @rdname INTERNAL_loading
#' @keywords internal
#'
#' @importFrom shiny showModal modalDialog
#' @importFrom htmltools div HTML
#'
loading <- function(msg) {
    showModal(modalDialog(
        title = "Loading",
        div(
            class = "progress",
            div(
                class = "progress-bar progress-bar-striped active",
                role = "progressbar",
                style = "width: 100%;"
            )
        ),
        HTML(paste0("<i>", msg, "</i>"))
    ))
}

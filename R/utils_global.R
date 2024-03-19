#' An wrapper function to handle errors and warnings
#' Will create a notification and add the exception to the global exception data
#' @param func function that is wrapped
#' @param component_name `str` name of the component (will be reported in the exception message)
#' @param ... arguments to be passed to the function
#'
#' @return Does not return anything but will create a notification and add the exception to the global exception data
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
                                "<b> Warning in ",
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
            suppressWarnings(func(...))
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
#' @return does not return anything but adds an exception to the global exception data
#' @rdname INTERNAL_add_exception
#' @keywords internal
#'
#' @importFrom shiny isolate
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
    old_data <- isolate(global_rv$exception_data)
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
#' @return does not return anything but will display a loading modal
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

#' A function that remove the "(scpGUI#x)" suffix from a string
#' @param string `str` string to remove the suffix from
#' @return `str` the string without the suffix
#' @rdname INTERNAL_remove_scpGUI
#' @keywords internal
#'
#'
remove_scpGUI <- function(string) {
    return(gsub("\\_(scpGUI#[0-9]+\\)", "", string))
}

#' PCA Methods Wrapper
#'
#' This function performs Principal Component Analysis (PCA) on a SingleCellExperiment object using the specified method.
#'
#' @param sce A SingleCellExperiment object. The PCA is performed on the assay of this object.
#' @param method A character string specifying the PCA method to use. This should be one of the methods supported by the pcaMethods package.
#'
#' @return A pcaRes object resulting from the PCA.
#' @rdname INTERNAL_pcaMethods_wrapper
#' @keywords internal
#'
#' @importFrom pcaMethods pca
#' @importFrom SummarizedExperiment assay
#'
pcaMethods_wrapper <- function(sce, method, transpose = FALSE) {
    mat <- assay(sce)
    mat <- mat[rowSums(is.na(mat)) != ncol(mat), ]
    mat <- mat[, colSums(is.na(mat)) < nrow(mat)]
    if (transpose) {
        mat <- t(mat)
    }
    pca <- pcaMethods::pca(mat,
        method = method
    )
    pca
}

#' A function that will subset the assays of a QFeatures object
#' @param qfeatures `QFeatures` object to subset
#' @param pattern `str` pattern to match the assays names
#' @return `QFeatures` object with the subsetted assays
#' @rdname INTERNAL_page_assays_subset
#' @keywords internal
#'
page_assays_subset <- function(qfeatures, pattern) {
    to_process <- grepl(
        pattern,
        names(qfeatures),
        fixed = TRUE
    )
    qfeatures[to_process]
}

#' Create a plotly PCA plot
#'
#' @param df a data.frame that contains the PCA results and the color column
#' @param color_name a character string that contains the name of the color column
#' @param pca_result a pcaRes object that contains the PCA results
#'
#' @return a plotly object
#' @rdname INTERNAL_pca_plotly
#' @keywords internal
#'
#' @importFrom plotly plot_ly layout %>% hide_colorbar config
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats as.formula
#' @importFrom viridisLite viridis
#'
pca_plotly <- function(df, pca_result, color_name, show_legend) {
    plotly <- plot_ly(df,
        x = ~PC1,
        y = ~PC2,
        color = as.formula(paste0("~", color_name)),
        text = ~Row.names,
        type = "scatter",
        mode = "markers",
        colors = if (is.numeric(df[[color_name]])) {
            viridisLite::viridis(10)
        } else {
            suppressWarnings(RColorBrewer::brewer.pal(
                length(unique(df[[color_name]])),
                "Set2"
            ))
        },
        hovertemplate = paste(
            "%{text}<br>",
            paste0(color_name, ": %{customdata}<extra></extra>")
        ),
        customdata = as.formula(paste0("~", color_name))
    ) %>%
        layout(
            xaxis = list(title = paste(
                "PC1",
                round(pca_result@R2[1] * 100, 2),
                "% of the variance"
            )),
            yaxis = list(title = paste(
                "PC2",
                round(pca_result@R2[2] * 100, 2),
                "% of the variance"
            )),
            showlegend = show_legend,
            legend = list(
                x = 1,
                y = 1,
                traceorder = "normal",
                font = list(
                    family = "sans-serif",
                    size = 10,
                    color = "black"
                ),
                bgcolor = "#E2E2E2",
                bordercolor = "#FFFFFF",
                borderwidth = 2
            )
        ) %>%
        config(displaylogo = FALSE, toImageButtonOptions = list(
            format = "svg",
            filename = "pca_plot",
            height = 500,
            width = 700,
            scale = 1
        ))
    if (!show_legend) {
        plotly <- plotly %>% hide_colorbar()
    }
    return(plotly)
}

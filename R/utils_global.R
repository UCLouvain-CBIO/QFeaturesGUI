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


#' Will convert a qfeatures object to a summary data.frame object
#'
#' @param qfeatures a qfeatures object
#'
#' @return a data.frame object
#' @rdname INTERNAL_qfeatures_to_df
#' @keywords internal
#'
qfeatures_to_df <- function(qfeatures) {
    df <- data.frame(
        "Name" = rep.int(0, length(qfeatures)),
        "Class" = rep.int(0, length(qfeatures)),
        "nrows" = rep.int(0, length(qfeatures)),
        "ncols" = rep.int(0, length(qfeatures))
    )
    for (i in seq_along(qfeatures)) {
        df[i, "Name"] <- remove_QFeaturesGUI(names(qfeatures)[[i]])
        df[i, "Class"] <- class(qfeatures[[i]])[[1]]
        df[i, "nrows"] <- nrow(qfeatures[[i]])[[1]]
        df[i, "ncols"] <- ncol(qfeatures[[i]])[[1]]
    }

    df
}


#' A function that remove the "(QFeaturesGUI#x)" suffix from a string
#' @param string `str` string to remove the suffix from
#' @return `str` the string without the suffix
#' @rdname INTERNAL_remove_QFeaturesGUI
#' @keywords internal
#'
#'
remove_QFeaturesGUI <- function(string) {
    return(gsub("_\\(QFeaturesGUI#[0-9]+\\)", "", string))
}
#' PCA Methods Wrapper
#'
#' This function performs Principal Component Analysis (PCA) on a SingleCellExperiment object using the specified method.
#'
#' @param sce A SingleCellExperiment object. The PCA is performed on the assay of this object.
#' @param method A character string specifying the PCA method to use. This should be one of the methods supported by the pcaMethods package.
#' @param center A logical indicating whether the variables should be centered before PCA.
#' @param scale A logical indicating whether the variables should be scaled before PCA.
#'
#' @return A pcaRes object resulting from the PCA.
#' @rdname INTERNAL_pcaMethods_wrapper
#' @keywords internal
#'
#' @importFrom pcaMethods pca
#' @importFrom SummarizedExperiment assay
#'
pcaMethods_wrapper <- function(sce, method, center, scale, transpose = FALSE) {
    mat <- assay(sce)
    mat <- mat[rowSums(is.na(mat)) != ncol(mat), ]
    mat <- mat[, colSums(is.na(mat)) < nrow(mat)]
    if (scale) {
        mat <- scale(mat)
    }
    if (transpose) {
        mat <- t(mat)
    }
    pca <- pcaMethods::pca(mat,
        method = method,
        center = center
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
    suppressMessages(suppressWarnings(qfeatures[, , to_process]))
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
    stopifnot(is.data.frame(df))
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
                "Set1"
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
        plotly <- hide_colorbar(plotly)
    }
    return(plotly)
}


#' A function that will add the assays to the global_rv qfeatures object
#'
#' @param processed_qfeatures `QFeatures` object to add to the global_rv qfeatures object
#' @param step_number `int` number of the step
#' @rdname INTERNAL_add_assays_to_global_rv
#' @keywords internal
#'
#' @return (NULL) does not return anything but will add the assays to the global_rv qfeatures object
#' @importFrom QFeatures addAssayLink

add_assays_to_global_rv <- function(processed_qfeatures, step_number, type) {
    for (name in names(processed_qfeatures)) {
        new_name <- paste0(
            strsplit(name, "_(QFeaturesGUI#", fixed = TRUE)[[1]][[1]],
            "_(QFeaturesGUI#", step_number, ")",
            "_", type, "_", step_number
        )

        global_rv$qfeatures[[new_name]] <- processed_qfeatures[[name]]

        global_rv$qfeatures <- addAssayLink(
            global_rv$qfeatures,
            from = name,
            to = new_name
        )
    }
}


#' A function that will logTransform all the assays of a qfeatures
#' @param qfeatures `QFeatures` object to logTransform
#' @param base `numeric` base of the log transformation
#' @param pseudocount `numeric` pseudocount to add to the data
#' @return `QFeatures` object with the log transformed assays
#' @rdname INTERNAL_log_transform_qfeatures
#' @keywords internal
#' @importFrom QFeatures logTransform QFeatures
#' @importFrom SummarizedExperiment colData
#'

log_transform_qfeatures <- function(qfeatures, base, pseudocount) {
    el <- lapply(names(qfeatures), function(name) {
        QFeatures::logTransform(
            object = qfeatures[[name]],
            base = base,
            pc = pseudocount
        )
    })
    names(el) <- names(qfeatures)
    QFeatures(el, colData = colData(qfeatures))
}

#' A function that will normalise all the assays of a qfeatures
#'
#' @param qfeatures `QFeatures` object to normalise
#' @param method `str` method to use for the normalisation (see `QFeatures::normalise`)
#' @return `QFeatures` object with the normalised assays
#' @rdname INTERNAL_normalisation_qfeatures
#' @keywords internal
#' @importFrom QFeatures normalize QFeatures
#' @importFrom SummarizedExperiment colData

normalisation_qfeatures <- function(qfeatures, method) {
    el <- lapply(names(qfeatures), function(name) {
        QFeatures::normalize(
            object = qfeatures[[name]],
            method = method
        )
    })
    names(el) <- names(qfeatures)
    QFeatures(el, colData = colData(qfeatures))
}


#' A function that return a plot of the densities of intensities by sample
#'
#' @param qfeatures `QFeatures` object
#' @param color `str` colname of the column of colData to use as color
#' @return a plotly object
#'
#' @rdname INTERNAL_density_by_sample_plotly
#' @keywords internal
#' @importFrom SummarizedExperiment assay colData

density_by_sample_plotly <- function(qfeatures, color) {
    combined_df <- data.frame(intensity = numeric(), sample = character())
    for (assayName in names(qfeatures)) {
        assayData <- assay(qfeatures[[assayName]])

        intensities <- as.vector(assayData)
        sampleNames <- rep(colnames(assayData), each = nrow(assayData))

        assay_df <- data.frame(intensity = intensities, sample = sampleNames)

        combined_df <- rbind(combined_df, assay_df)
    }

    combined_df$color <- colData(qfeatures)[combined_df$sample, color]

    plotlyridges(
        data = combined_df,
        vardens = "intensity",
        varcat = "sample"
    )
}

# This function comes from the github repo rushkin/bitsandends
# Thanks to iliarushkin for this function

#' Mimicks \code{ggridges} but using \code{plotly} so that the output is interactive
#'
#' @param data data-frame of data to plot
#' @param vardens name of the column in \code{data} to use on the x-axis ('density variable')
#' @param varcat name of the column in \code{data} to use for splitting plots ('category variable')
#' @param linecolor line color
#' @param fillcolor fill color
#' @param fillopacity fill opacity
#' @param linewidth line width
#' @param scale vertical scale of plots, compared to the spacing between plots.
#' @param logspaced boolean, whether to use log-spaced points in calculating density
#' @param cut.from how much to cut into the region to the left of the smallest data values, in units of the bandwidth.
#' @param cut.to how much to cut into the region to the right of the greatest data values, in units of the bandwidth.
#' @param n number of points used in calculation of density
#' @param bw bandwidth to use in density calculation. if NULL, uses the default of \code{density}.
#' @param bw.separate if TRUE, will use separately estimated bandwidth in each plot, overriding \code{bw}
#' @param height.norm vertical normalization of plots. If 'integral', will normalize to unit area under the curve. If '1' will normalize to unit maximum height
#' @param round.digits number of rounding digits used in hover labels
#' @param x.min lower end of the x-axis range
#' @param height passed as \code{height} to the plotly object
#' @param width passed as \code{width} to the plotly object
#' @return a plotly object
#'
#' @rdname INTERNAL_plotlyridges
#' @keywords internal
#' @importFrom plotly plot_ly add_trace layout
#'
plotlyridges <- function(
        data, vardens, varcat, linecolor = "darkblue", fillcolor = "steelblue", fillopacity = 0.6, linewidth = 0.5, scale = 0.9, logspaced = FALSE, cut.from = 0, cut.to = 3, n = 512, bw = NULL, bw.separate = FALSE, height.norm = "integral", round.digits = 2, x.min = 0,
        height = NULL,
        width = NULL) {
    data <- subset(data, !is.na(data[, vardens]))

    r <- range(data[, vardens])

    if (is.null(bw)) {
        if (!bw.separate) {
            if (logspaced) {
                x <- log(data[, vardens])
            } else {
                x <- data[, vardens]
            }
            if (length(x) > 1) {
                bw <- density(x)$bw
            } else {
                bw <- 1
            }
        }
    }

    df <- aggregate(data[, vardens], by = list(varcat = data[, varcat]), FUN = function(x) {
        if (length(x) == 0) {
            return(NULL)
        }
        if (bw.separate) {
            if (length(x) == 1) {
                bw <- 1
            } else {
                bw <- "nrd0"
            }
        }
        if (logspaced) {
            x <- log(x)
        }
        from <- min(x, na.rm = TRUE) - cut.from * bw
        to <- max(x, na.rm = TRUE) + cut.to * bw
        d <- density(x, bw = bw, n = n, from = from, to = to)[1:2]
        # Normalize height
        if (height.norm == "1") {
            d$y <- d$y / max(d$y)
        }
        if (height.norm == "integral") {
            d$y <- d$y / (sum(d$y) * (d$x[2] - d$x[1]))
        }

        if (logspaced) {
            d$x <- exp(d$x)
            d$y <- d$y / d$x
        }


        return(d)
    })

    text <- aggregate(data[, vardens], by = list(varcat = data[, varcat]), FUN = function(x) {
        x <- x[!is.na(x)]
        q <- quantile(x)
        text <- paste0("Observations: ", prettyNum(length(x), big.mark = ","), "<br>Median: ", round(q[3], round.digits), "<br>Range: [", round(q[1], round.digits), ", ", round(q[5], round.digits), "]", "<br>Interquartile Range: [", round(q[2], round.digits), ", ", round(q[4], round.digits), "]")
        return(text)
    })$x




    catnames <- df[[1]]
    x <- df[[2]][1:(length(df[[1]]))]
    y <- df[[2]][-(1:(length(df[[1]])))]



    ymax <- max(unlist(y), na.rm = TRUE)
    y <- lapply(y, function(yy) {
        yy <- scale * yy / ymax
        return(yy)
    })
    p <- plotly::plot_ly(type = "scatter", mode = "lines", height = height, width = width)

    fillcolor <- as.vector(col2rgb(fillcolor)) / 255
    fillcolor <- rgb(fillcolor[1], fillcolor[2], fillcolor[3], alpha = fillopacity)


    if (is.null(x.min)) {
        xaxis <- list(range = r)
    } else {
        xaxis <- list(range = c(x.min, r[2]))
    }

    for (i in rev(1:length(catnames))) {
        p <- plotly::add_trace(p, x = x[[i]], y = i, line = list(color = linecolor, width = linewidth), showlegend = FALSE, hoverinfo = "none")
        p <- plotly::add_trace(p,
            x = x[[i]], y = y[[i]] + i, fill = "tonexty", fillcolor = fillcolor, line = list(color = linecolor, width = linewidth), showlegend = FALSE, name = catnames[i], hoverinfo = "text", text = text[i]
        )

        p <- plotly::layout(p,
            yaxis = list(tickmode = "array", tickvals = (1:length(catnames)), ticktext = catnames, showline = TRUE),
            xaxis = xaxis
        )
    }
    p
    return(p)
}

#' A function that create a data frame that contains the intensities, the sample names (+ one col of colData and one col of rowData)
#'
#' @param qfeatures `QFeatures` object
#' @param sample_column `str` column of colData to use as sample names
#' @param feature_column `str` column of rowData to use as feature names
#' @return a data.frame
#'
#' @rdname INTERNAL_summarize_assays_to_df
#' @keywords internal
#' @importFrom SummarizedExperiment assay colData rowData
#' @importFrom tidyr pivot_longer
#'

summarize_assays_to_df <- function(qfeatures, sample_column, feature_column = NULL) {

    combined_df <- data.frame(PSM = character(), intensity = numeric(), sample = character())
    for (assayName in names(qfeatures)) {
        assayData <- as.data.frame(assay(qfeatures[[assayName]]))

        assayData <- pivot_longer(assayData, everything(), names_to = "sample", values_to = "intensity")
        assayData$PSM <- rownames(assayData)

        matched_indices <- match(assayData$sample, rownames(colData(qfeatures)))
        length(matched_indices)
        assayData$sample_type <- colData(qfeatures)[matched_indices, sample_column]

        if (!is.null(feature_column)) {
            matched_indices <- match(assayData$PSM, rownames(rowData(qfeatures[[assayName]])))
            assayData$feature_type <- rowData(qfeatures[[assayName]])[matched_indices, feature_column]
        }
        combined_df <- rbind(combined_df, assayData)
    }
    combined_df
}

#' A function that return the boxplot of the intensities of all features by an sample annotation
#'
#' @param assays_df a data.frame that contains the intensities, the sample names (+ one col of colData and one col of rowData)
#'
#' @return a plot
#'
#' @rdname INTERNAL_feature_boxplot
#' @keywords internal
#' @importFrom plotly ggplotly
#' @importFrom ggplot2 ggplot aes geom_violin
#'

features_boxplot <- function(assays_df) {
    plot <- ggplot(assays_df, aes(
        x = sample_type,
        y = intensity,
        colour = sample_type,
        fill = sample_type
    )) +
        geom_violin(aes(alpha = 0.5))

    suppressWarnings(ggplotly(plot))
}

#' A function that will return the boxplot of the intensities of an individual feature by a sample annotation
#'
#' @param assays_df a data.frame that contains the intensities, the sample names (+ one col of colData and one col of rowData)
#' @param feature `str` feature to plot
#'
#' @return a plot
#'
#' @rdname INTERNAL_unique_feature_boxplot
#' @keywords internal
#' @importFrom plotly ggplotly
#' @importFrom ggplot2 ggplot aes geom_boxplot
#'

unique_feature_boxplot <- function(assays_df, feature) {
    plot <- ggplot(assays_df[assays_df$feature_type == feature, , drop = FALSE], aes(x = sample_type, y = intensity, colour = sample_type)) +
        geom_boxplot()

    suppressWarnings(ggplotly(plot))
}

#' A function that write code lines
#'
#' @param input_table a reactiveVal containing the input table
#' @param sample_table a reactiveVal containing the sample table
#' @param qfeature a QFeatures Object generate by the eventReactive of server_module_box_readQFeatures
#' @param run_col a variable containing the column name containing batches(can be NULL)
#' @param removeEmptyCols a boolean to remove empty column from QFeatures Object
#' @param quant_cols a variable that indicate which column can be used if there is no quantCols in sample_table
#' @param logTransform a boolean that indicate if logTransformation should be perform or not
#' @param zero_as_na a boolean indicating if zero should considered as NA values
#'
#' @return A vector containing code lines
#' @rdname INTERNAL_code_generator_importQFeatures
#' @keywords internal
#'
#' @importFrom shiny is.reactive reactive moduleServer observe eventReactive
#' @importFrom methods as
#' @import SingleCellExperiment
#' @import SummarizedExperiment
#' @import MultiAssayExperiment
#'
code_generator_importQFeatures <- function(input_table, sample_table, qfeatures, run_col, removeEmptyCols, quant_cols, logTransform, zero_as_NA) {
    codeLines <- c()
    if (is.data.frame(sample_table())) {
        if (run_col != "NULL") {
            codeLines <- c(codeLines, sprintf(
                "qfeatures <- QFeatures::readQFeatures(
                assayData = input_table,
                colData = sample_table,
                runCol = '%s',
                removeEmptyCols = %s,
                verbose = FALSE)",
                run_col,
                removeEmptyCols
            ))
        } else {
            codeLines <- c(codeLines, sprintf(
                "qfeatures <- QFeatures::readQFeatures(
                assayData = input_table,
                colData = sample_table,
                runCol = NULL,
                removeEmptyCols = %s,
                verbose = FALSE
                )",
                removeEmptyCols
            ))
        }
    } else {
        if (run_col != "NULL") {
            codeLines <- c(codeLines, sprintf(
                "qfeatures <- QFeatures::readQFeatures(
           assayData = input_table,
           runCol = '%s',
           quantCols = '%s',
           removeEmptyCols = %s,
           verbose = FALSE
           )",
                run_col,
                quant_cols,
                removeEmptyCols
            ))
        } else {
            codeLines <- c(codeLines, sprintf(
                "qfeatures <- QFeatures::readQFeatures(
                  assayData = input_table,
                  runCol = NULL,
                  quantCols = '%s',
                  removeEmptyCols = %s,
                  verbose = FALSE
                  )",
                quant_cols,
                removeEmptyCols
            ))
        }
    }
    if (zero_as_NA && length(qfeatures) > 0) {
        codeLines <- c(codeLines, "qfeatures <- QFeatures::zeroIsNA(
                    object = qfeatures,
                    i = seq_along(qfeatures))")
    }
    if (logTransform) {
        codeLines <- c(codeLines, "qfeatures <- QFeatures::logTransform(
        object = qfeatures,
        i = seq_along(qfeatures),
        base = 2,
        name = paste0(names(qfeatures), '_logTransformed'))")
    }
    codeLines
}

code_generator_read_table <- function(id, file, sep, dec, skip, stringAsFactors, comment) {
    codeLines <- c()
    codeLines <- c(codeLines, sprintf(
        "%s_table <- read.table(%s,
    sep = '%s',
    dec = '%s',
    skip = '%s',
    stringsAsFactors = %s,
    comment.char = '%s',
    header = TRUE,
    row.names = 1
    )", id,
        file,
        sep,
        dec,
        skip,
        stringAsFactors,
        comment
    ))
    codeLines
}

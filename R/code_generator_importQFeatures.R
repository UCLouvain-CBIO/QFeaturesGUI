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


code_generator_importQFeatures <- function(input_table, sample_table, qfeatures, run_col, removeEmptyCols, quant_cols, logTransform, zero_as_NA) {
    if (is.data.frame(sample_table())) {
        colData <- "sample_table"
        quantCols <- "NULL"
    } else {
        colData <- "NULL"
        quantCols <- quant_cols
    }

    if (run_col != "NULL") {
        runCol <- paste0("'", run_col, "'")
    } else {
        runCol <- "NULL"
    }

    quantColumns <- paste(sprintf('"%s"', quantCols), collapse = ",\n\t\t")
    codeLines <- sprintf(
        "\nqfeatures <- QFeatures::readQFeatures(\n\tassayData = input_table,\n\tcolData = %s,\n\trunCol = %s,\n\tquantCols = c(%s),\n\tremoveEmptyCols = %s,\n\tverbose = FALSE\n)",
        colData,
        runCol,
        quantColumns,
        removeEmptyCols
    )
    if (zero_as_NA && length(qfeatures) > 0) {
        codeLines <- c(codeLines, "\nqfeatures <- QFeatures::zeroIsNA(\n\tobject = qfeatures,\n\ti = seq_along(qfeatures)\n)")
    }
    if (logTransform) {
        codeLines <- c(codeLines, "\nqfeatures <- QFeatures::logTransform(\n\tobject = qfeatures,\n\ti = seq_along(qfeatures),\n\tbase = 2,\n\tname = paste0(names(qfeatures), '_logTransformed')\n)")
    }
    codeLines
}

code_generator_read_table <- function(id, file, sep, dec, skip, stringAsFactors, comment) {
    codeLines <- c()
    codeLines <- c(codeLines, sprintf(
        "\n%s_table <- read.table(%s,\n\tsep = '%s',\n\tdec = '%s',\n\tskip = '%s',\n\tstringsAsFactors = %s,\n\tcomment.char = '%s',\n\theader = TRUE,\n\trow.names = 1\n)",
        id,
        file,
        sep,
        dec,
        skip,
        stringAsFactors,
        comment
    ))
    codeLines
}

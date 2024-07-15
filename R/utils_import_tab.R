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

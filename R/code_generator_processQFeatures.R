#' @title Code generator for aggregation tab
#' @param method the method used to do the aggregation
#' @param fcol `character(1)` naming a `rowData` variable that defines how to aggregate
#'   the features within each assay. This variable is either a character or a (possibly
#'   sparse) matrix.
#'
#' @return code line generated
#' @rdname codeGeneratorAggregation
#' @keywords internal
#'

codeGeneratorAggregation <- function(method, fcol){
  codeLines <- sprintf(
    "qf <- lapply(seq_along(qf), function(i) {
  \tname <- names(qf)[i]
  \taggregateFeatures(
  \t\tobject = qf[[name]],
  \t\tmethod = %s,
  \t\trunCol = '%s',
  \t\tna.rm = TRUE
  \t)
  })",
    method,
    fcol
  )
  codeLines
}

#' @title Code generator for join tab
#'
#' @return code line generated
#' @rdname codeGeneratorJoin
#' @keywords internal
#'

codeGeneratorJoin <- function(){
  codeLines <- sprintf(
    "qf <- joinAssays(
  \tx = qf,
  \ti = names(qf)
  )"
  )
  codeLines
}

#' @title Code generator for filtering missing values tab
#' @param pNA `float` threshold for filtering missing value
#' @param type feature or sample
#'
#' @return code lines generated
#' @rdname codeGeneratorNA
#' @keywords internal
#'

codeGeneratorNA <- function(pNA, type){
  if(type == "features"){
    codeLines <- sprintf(
      "qf <- filterNA(
      \tobject = qf,
      \ti = seq_along(qf),
      \tpNA = %s
      )",
      pNA
    )
  } else {
    codeLines <- sprintf(
      "tableNA <- nNA(
      \tobject = qf,
      \ti = seq_along(qf)
      \t)\ntableMetadata <- colData(qf)\ntableMetadata$pNA <- tableNA$nNAcols$pNA[match(rownames(df_to_render), tableNA$nNAcols$name)]\nqf <- qf[, tableMetadata$pNA <= %s,]",
      pNA
    )
  }
  codeLines
}

#' @title Code generator for normalisation tab
#' @param method the method used to do the normalisation
#' 
#' @return code lines generated
#' @rdname codeGeneratorNormalisation
#' @keywords internal
#'

codeGeneratorNormalisation <- function(method){
  codeLines <- sprintf(
    "qf <- lapply(names(qf), function(name){
    \tnormalize(
    \t\tobject = qf[[name]],
    \t\tmethod = '%s'
    \t)\n})",
    method
  )
  codeLines
}

#' @title Code generator for filtering tab
#' @param condition A list of filtering condition specifications
#' @param type feature or sample
#'
#' @return code lines generated
#' @rdname codeGeneratorFiltering
#' @keywords internal
#'

codeGeneratorFiltering <- function(condition, type){
  if(length(condition) == 0){
    codeLines <- sprintf("#No %s filtering applied", type)
  } else {
    if(type == "features"){
      final = ""
      for(i in 1:length(condition)){
        build_condition <- paste0("filterFeatures(~",condition[[i]]$annotation, " ", condition[[i]]$operator," ")
        if(is.numeric(condition[[i]]$value[[1]])){
          vector <- paste0("c(", paste(condition[[i]]$value, collapse = ","), "))")
        } else{
          vector <- paste0("c('", paste(condition[[i]]$value, collapse = "','"), "'))")
        }
        build_condition <- paste0(build_condition, vector)
        final <- paste0(final, " |> ", build_condition)
      }
      condition_used <- paste0("qf", final)
      codeLines <- sprintf(
        "qf <- %s",
        condition_used
      )
    } else {
      final = ""
      for(i in 1:length(condition)){
        build_condition <- paste0("colData(qf)$", condition[[i]]$annotation, " ", condition[[i]]$operator, " ")
        if(is.numeric(condition[[i]]$value[[1]])){
          vector <- paste0("c(", paste(condition[[i]]$value, collapse = ","), ")")
        } else{
          vector <- paste0("c('", paste(condition[[i]]$value, collapse = "','"), "')")
        }
        build_condition <- paste0(build_condition, vector)
        if(i == 1){
          final <- build_condition
        } else {
          final <- paste0(final, " & ", build_condition)
        }
      }
      condition_used <- paste0("qf[, ", final, "]")
      codeLines <- sprintf(
        "qf <- %s",
        condition_used
      )
    }
  }
  codeLines
}

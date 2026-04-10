codeGeneratorInitialization <- function(qf, step_number){
  vec <- names(qf)
  if(step_number == 1){
    initial_setNames <- vec[grep(pattern = paste0("QFeaturesGUI#0"), vec)]
    initial_setNames <- gsub("_\\(QFeaturesGUI#[0-9]+\\)", "", initial_setNames)
    step_setNames <- vec[grep(pattern = paste0("QFeaturesGUI#", step_number), vec)]
    step_setNames <- gsub("_\\(QFeaturesGUI#[0-9]+\\)", "", step_setNames)
    codeLines <- sprintf(
      "####################################
######### initial set names ########
####################################
step0_setNames <- c(%s)\n
####################################
####### Step number %s names ######## 
####################################
step%s_setNames <- c(%s)\n",
      paste(sprintf('"%s"', initial_setNames), collapse = ", \n\t"),
      step_number, 
      step_number, 
      paste(sprintf('"%s"', step_setNames), collapse = ", \n\t")
      )
  } else {
    step_setNames <- vec[grep(pattern = paste0("QFeaturesGUI#", step_number), vec)]
    step_setNames <- gsub("_\\(QFeaturesGUI#[0-9]+\\)", "", step_setNames)
    codeLines <- sprintf(
      "####################################
####### Step number %s names ########
####################################
step%s_setNames<- c(%s)\n",
      step_number,
      step_number,
      paste(sprintf('"%s"', step_setNames), collapse = ", \n\t")
      )
  }
  codeLines
}

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
  "####################################
########### Aggregation ############
####################################\n
qf <- lapply(seq_along(qf), function(i) {
\tname <- names(qf)[i]
\taggregateFeatures(
\t\tobject = qf[[name]],
\t\tfun = %s,
\t\tfcol = '%s',
\t\tna.rm = TRUE
\t)
})\n",
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
"####################################
############### Join ###############
####################################\n
qf <- joinAssays(
\tx = qf,
\ti = names(qf)
)\n"
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

codeGeneratorNA <- function(pNA, type, step_number){
  if(type == "features"){
    codeLines <- sprintf(
      "####################################
###### Missing value features ######
####################################\n
for (i in 1:length(step%s_setNames)){
\tqf[[step%s_setNames[i]]] <- filterNA(
\t\tobject = qf[[step%s_setNames[i]]],
\t\tpNA = %s
\t)
}",
      step_number,
      step_number,
      step_number-1,
      pNA
    )
  } else {
    codeLines <- sprintf(
      "####################################
###### Missing value samples #######
####################################\n
for(i in 1:length(step%s_setNames)){
  tableNA <- nNA(
    object = qf,
    i = step%s_setNames[i]
  )
  tableMetadata <- colData(qf[[step%s_setNames[i]]])
  tableMetadata$pNA <- tableNA$nNAcols$pNA[match(rownames(tableMetadata), tableNA$nNAcols$name)]
  qf[[step%s_setNames[i]]] <- qf[, tableMetadata$pNA <= %s,]
}",
      step_number,
      step_number-1,
      step_number-1,
      step_number,
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
    "####################################
########## Normalisation ###########
####################################\n
qf <- lapply(names(qf), function(name){
\tnormalize(
\t\tobject = qf[[name]],
\t\tmethod = '%s'
\t)\n})\n",
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
    codeLines <- sprintf(
      "####################################
######## %s filtering ########
####################################\n
#No %s filtering applied\n",
      type,
      type)
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
        "####################################
######## features filtering ########
####################################\n
qf <- %s\n",
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
        "####################################
######## samples filtering #########
####################################\n
qf <- %s\n",
        condition_used
      )
    }
  }
  codeLines
}

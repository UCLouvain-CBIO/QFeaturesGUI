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

############################################## Add a function that try if the step number have the same number of set if not render a new list updated with the right name of sets

check_for_missing_set <- function(qf,step_number){
  vec <- names(qf)
  indice_to_remove <- c()
  initial_setNames <- vec[grep(pattern = paste0("QFeaturesGUI#",step_number-1), vec)]
  initial <- gsub("_\\(QFeaturesGUI#[0-9]+\\)_*[a-z]*_*[a-z]*_*[0-9]*", "", initial_setNames)
  currentStep_setNames <- vec[grep(pattern = paste0("QFeaturesGUI#",step_number),vec)]
  current <- gsub("_\\(QFeaturesGUI#[0-9]+\\)_*[a-z]*_*[a-z]*_*[0-9]*", "", currentStep_setNames)
   if(length(initial)!= length(current)){
     for(i in 1:length(initial)){
       if(!(initial[i] %in% current)){
         indice_to_remove <- append(indice_to_remove,i)
       }
     }
     initial_setNames <- initial_setNames[-indice_to_remove]
     initial_setNames <- remove_QFeaturesGUI(initial_setNames)
     codeLines <- sprintf("#After filtering steps one or more set has been deleted.\nstep%s_setNames <- c(%s)\n", 
                          step_number-1,
                          paste(sprintf('"%s"', initial_setNames), collapse = ", \n\t"))
     return(codeLines)
   }
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

codeGeneratorAggregation <- function(method, fcol, step_number){
  codeLines <- sprintf(
  "####################################
########### Aggregation ############
####################################\n
for(i in 1:length(step%s_setNames)){
  qf[[step%s_setNames[i]]] <- aggregateFeatures(
    object = qf[[step%s_setNames[i]]],
    fun = %s,
    fcol = '%s',
    na.rm = TRUE
  )
}\n",
  step_number-1,
  step_number,
  step_number-1,
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

codeGeneratorJoin <- function(step_number){
  codeLines <- sprintf(
"####################################
############### Join ###############
####################################\n
qf <- joinAssays(
\tx = qf,
\ti = step%s_setNames,
\tname = step%s_setNames
)\n",
  step_number-1,
  step_number
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
  qf[[step%s_setNames[i]]] <- qf[[step%s_setNames[i]]][, tableMetadata$pNA <= %s]
}",
      step_number,
      step_number-1,
      step_number-1,
      step_number,
      step_number-1,
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

codeGeneratorNormalisation <- function(method, step_number){
  codeLines <- sprintf(
    "####################################
########## Normalisation ###########
####################################\n
for(i in 1:length(step%s_setNames)){
qf[[step%s_setNames[i]]] <- normalize(
\tobject = qf[[step%s_setNames[i]]],
\tmethod = '%s'
\t)}\n",
    step_number-1,
    step_number,
    step_number-1,
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

codeGeneratorFiltering <- function(qf,condition, type, step_number){
  codeLines <- check_for_missing_set(qf, step_number = step_number)
  if(length(condition) == 0){
    codeLines <- c(codeLines,sprintf(
      "####################################
######## %s filtering ########
####################################\n
#No %s filtering applied\n",
      type,
      type))
  } else {
    if(type == "features"){
      final = "se <- se["
      for(i in 1:length(condition)){
        if(condition[[i]]$operator == "=="){
          build_condition <- paste0("rowData(se)$", condition[[i]]$annotation, " %in% ")
          if(is.numeric(condition[[i]]$value[[1]])){
            vector <- paste0("c(", paste(condition[[i]]$value, collapse = ","), ")")
          } else {
            vector <- paste0("c('", paste(condition[[i]]$value, collapse = "','"), "')")
          }
        } else if(condition[[i]]$operator == "!="){
          build_condition <- paste0("!(rowData(se)$", condition[[i]]$annotation, " %in% ")
          if(is.numeric(condition[[i]]$value[[1]])){
            vector <- paste0("c(", paste(condition[[i]]$value, collapse = ","), "))")
          } else {
            vector <- paste0("c('", paste(condition[[i]]$value, collapse = "','"), "'))")
          }
        } else {
          build_condition <- paste0("rowData(se)$", condition[[i]]$annotation, " ", condition[[i]]$operator, " ")
          if(is.numeric(condition[[i]]$value[[1]])){
            vector <- paste0("c(", paste(condition[[i]]$value, collapse = ","), ")")
          } else {
            vector <- paste0("c('", paste(condition[[i]]$value, collapse = "','"), "')")
          }
        }
        build_condition <- paste0(build_condition, vector)
        if(i == 1){
          final <- paste0(final,build_condition)
        } else {
          final <- paste0(final, " & ", build_condition)
        }
      }
      condition_used <- paste0(final, ",]")
      print(condition_used)
      codeLines <- c(codeLines, sprintf(
        "####################################
######## features filtering ########
####################################\n
for(i in 1:length(step%s_setNames)){
\tse <- getWithColData(qf, step%s_setNames[i])
\t%s
\tqf[[step%s_setNames[i]]] <- se
}\n",
        step_number-1,
        step_number-1,
        condition_used,
        step_number
      ))
    } else {
      final = "se <- se[,"
      for(i in 1:length(condition)){
        if(condition[[i]]$operator == "=="){
          build_condition <- paste0("colData(se)$", condition[[i]]$annotation, " %in% ")
          if(is.numeric(condition[[i]]$value[[1]])){
            vector <- paste0("c(", paste(condition[[i]]$value, collapse = ","), ")")
          } else {
            vector <- paste0("c('", paste(condition[[i]]$value, collapse = "','"), "')")
          }
        } else if(condition[[i]]$operator == "!="){
          build_condition <- paste0("!(colData(se)$", condition[[i]]$annotation, " %in% ")
          if(is.numeric(condition[[i]]$value[[1]])){
            vector <- paste0("c(", paste(condition[[i]]$value, collapse = ","), "))")
          } else {
            vector <- paste0("c('", paste(condition[[i]]$value, collapse = "','"), "'))")
          }
        } else {
          build_condition <- paste0("colData(se)$", condition[[i]]$annotation, " ", condition[[i]]$operator, " ")
          if(is.numeric(condition[[i]]$value[[1]])){
            vector <- paste0("c(", paste(condition[[i]]$value, collapse = ","), ")")
          } else {
            vector <- paste0("c('", paste(condition[[i]]$value, collapse = "','"), "')")
          }
        }
        build_condition <- paste0(build_condition, vector)
        if(i == 1){
          final <- paste0(final,build_condition)
        } else {
          final <- paste0(final, " & ", build_condition)
        }
      }
      condition_used <- paste0(final, "]")
      codeLines <- c(codeLines, sprintf(
        "####################################
######## samples filtering #########
####################################\n
for(i in 1:length(step%s_setNames)){
\tse <- getWithColData(qf, step%s_setNames[i])
\t%s
\tqf[[step%s_setNames[i]]] <- se
}\n",
        step_number-1,
        step_number-1,
        condition_used,
        step_number
      ))
    }
  }
  codeLines
}

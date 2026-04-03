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


codeGeneratorJoin <- function(){
  codeLines <- sprintf(
    "qf <- joinAssays(
    \tx = qf,
    \ti = names(qf)
    )"
  )
  codeLines
}


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


codeGeneratorFiltering <- function(condition, type){
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
  codeLines
}

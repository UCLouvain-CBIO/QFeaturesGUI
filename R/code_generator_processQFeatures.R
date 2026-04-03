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

### Re do this function 
codeGeneratorFiltering <- function(qf, condition, type){
  if(type == "feature"){
    codeLines <- sprintf(
      "for(assay_name in names(%s)) {
      \t%s[[assay_name]] <- %s[%s, drop = FALSE]
      }",
      qf,
      qf,
      qf,
      condition
    )
  } else {
    codeLines <- sprintf(
      "%s <- %s[, %s, ]",
      qf,
      qf,
      condition
    )
  }
  codeLines
}

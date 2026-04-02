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


codeGeneratorNA <- function(qf, pNA, type, tableMetadataNA = NULL){
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
      "qf <- qf[, qf$pNA <= %s,]",
      pNA
    )
  }
  codeLines
}


codeGeneratorNormalisation <- function(qf, method){
  codeLines <- sprintf(
    "%s <- lapply(names(%s), function(name){
    \tnormalize(
    \t\tobject = %s[[name]],
    \t\tmethod = %s
    \t)
    })",
    qf,
    qf,
    qf,
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
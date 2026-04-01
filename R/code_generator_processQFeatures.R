codeGeneratorAggregation <- function(qf, method, fcol){
  codeLines <- sprintf(
    "%s <- lapply(seq_along(%s), function(i) {
    \tname <- names(%s)[i]
    \taggregateFeatures(
    \t\tobject = %s[[name]],
    \t\tmethod = %s,
    \t\trunCol = %s,
    \t\tna.rm = TRUE
    \t)
    }",
    qf,
    qf,
    qf,
    qf,
    method,
    fcol
  )
  codeLines
}


codeGeneratorJoin <- function(names){
  codeLines <- sprintf(
    "qf <- joinAssays(
    \t% x = qf,
    \t% i = %s
    )",
    names
  )
  codeLines
}


codeGeneratorNA <- function(qf, pNA, type, tableMetadataNA = NULL){
  if(type == "features"){
    codeLines <- sprintf(
      "%s <- filterNA(
     \tobject = %s,
     \ti = seq_along(%s),
     \tpNA = %s
     )",
      qf,
      qf,
      qf,
      pNA
    )
  } else {
    codeLines <- sprintf(
      "%s <- %s[, %s$pNA <= pNA,]"
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
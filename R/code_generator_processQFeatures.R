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


codeGeneratorJoin <- function(qf){
  codeLines <- sprintf(
    "%s <- joinAssays(
    \t% x = %s,
    \t% i = names(%s)
    )",
    qf,
    qf,
    qf
  )
}



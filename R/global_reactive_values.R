# enable the possibility to have global values shared across modules
#' @importFrom shiny reactiveValues
global_rv <- reactiveValues()

# Non-reactive global store for the QFeatures object. Using an environment
# allows mutation of its contents even in a locked package namespace,
# unlike top-level variables which cannot be reassigned with <<-.
.qf <- new.env(parent = emptyenv())
.qf$qfeatures <- NULL

#'
#' @title Workflow Configuration Module Server
#'
#' @param id The module id
#'
#' @return A Shiny module UI function
#' @rdname INTERNAL_server_module_workflow_config
#' @keywords internal
#' @importFrom shiny moduleServer observeEvent reactiveVal showModal modalDialog
#' @importFrom shiny modalButton actionButton removeModal tagList p
#' @importFrom shinyalert shinyalert
#'
#' @keywords internal
server_module_workflow_config <- function(id) {
    moduleServer(id, function(input, output, session) {
        session$sendCustomMessage(
            "initWorkflowSortable",
            list(
                palette  = session$ns("palette"),
                workflow = session$ns("workflow"),
                input    = session$ns("workflow_list")
            )
        )

        pending_config <- reactiveVal(NULL)

        observeEvent(input$apply, {
            any_saved <- !is.null(global_rv$step_rvs) &&
                any(vapply(global_rv$step_rvs, function(rv) rv() > 0L, logical(1)))

            if (any_saved) {
                pending_config(input$workflow_list)
                showModal(modalDialog(
                    title = "Reset processing progress?",
                    p(
                        "You have saved processing steps.",
                        "Changing the workflow will discard all processed assays",
                        "and you will need to re-run your processing from the beginning."
                    ),
                    footer = tagList(
                        modalButton("Cancel"),
                        actionButton(
                            session$ns("confirm_reset"),
                            "Reset and Apply",
                            class = "btn-danger"
                        )
                    )
                ))
            } else {
                global_rv$workflow_config <- input$workflow_list
                n <- length(input$workflow_list)
                shinyalert(
                    title = "Workflow applied",
                    text = paste0(
                        n, " step", if (n != 1) "s" else "",
                        " successfully configured."
                    ),
                    closeOnClickOutside = TRUE,
                    type = "success",
                    confirmButtonCol = "#3c8dbc"
                )
            }
        })

        observeEvent(input$confirm_reset, {
            # Strip .qf$qfeatures back to the initial state: keep original
            # assays (no suffix) and the initial sets (#0), remove all step
            # outputs (#1, #2, ...).
            keep <- !grepl("_\\(QFeaturesGUI#[1-9][0-9]*\\)", names(.qf$qfeatures))
            .qf$qfeatures <- .qf$qfeatures[, , keep]

            # Reset every step reactiveVal so downstream modules and the
            # sidebar return to their initial (unsaved) state
            lapply(global_rv$step_rvs, function(rv) rv(0L))

            global_rv$workflow_config <- pending_config()
            pending_config(NULL)
            removeModal()
            n <- length(global_rv$workflow_config)
            shinyalert(
                title = "Workflow reset & applied",
                text = paste0(
                    "Processing progress cleared.\n",
                    n, " step", if (n != 1) "s" else "",
                    " successfully configured."
                ),
                closeOnClickOutside = TRUE,
                type = "success",
                confirmButtonCol = "#3c8dbc"
            )
        })
    })
}

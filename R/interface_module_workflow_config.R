#'
#' @title Workflow Configuration Module (interface)
#'
#' @param id The module id
#'
#' @return A Shiny module UI function
#' @rdname INTERNAL_interface_module_workflow_config
#' @keywords internal
#'
#' @importFrom shiny uiOutput actionButton tagList icon
#' @importFrom htmltools div tags
#'
#' @title Workflow Configuration Module (interface)
#' @param id module id
#' @keywords internal
interface_module_workflow_config_tab <- function(id) {
    tagList(
        fluidRow(
            column(
                width = 4,
                box(
                    title = "Documentation",
                    width = 12,
                    solidHeader = FALSE,
                    status = "primary",
                    tags$p("test"),
                    collapsible = FALSE
                )
            ),
            column(
                width = 8,
                box(
                    title = "Workflow Configuration",
                    width = 12,
                    solidHeader = FALSE,
                    status = "primary",
                    tagList(
                        fluidRow(
                            column(
                                4,
                                div(
                                    class = "panel",
                                    tags$h4("Available steps"),
                                    div(
                                        id = NS(id, "palette"),
                                        class = "palette-container",
                                        lapply(
                                            c(
                                                "Samples Filtering",
                                                "Features Filtering",
                                                "Log Transformation",
                                                "Normalisation"
                                            ),
                                            function(s) div(class = "step", `data-step` = s, s)
                                        )
                                    )
                                )
                            ),
                            column(
                                8,
                                div(
                                    class = "panel",
                                    tags$h4("Workflow"),
                                    div(
                                        id = NS(id, "workflow"),
                                        class = "workflow-drop workflow-container",
                                        lapply(
                                            c("Samples Filtering", "Log Transformation"),
                                            function(s) {
                                                div(class = "step", `data-step` = s, s)
                                            }
                                        )
                                    )
                                )
                            ),
                            fluidRow(
                                column(
                                    width = 12,
                                    align = "center",
                                    actionButton(
                                        NS(id, "apply"),
                                        "Confirm Current Workflow",
                                        class = "load-button",
                                        width = "80%"
                                    )
                                )
                            ),
                            tags$script(src = "app-assets/sortable.min.js"),
                            tags$script(src = "app-assets/workflow_sortable.js")
                        )
                    )
                )
            )
        )
    )
}

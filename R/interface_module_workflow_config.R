#'
#' @title Workflow Configuration Module (interface)
#'
#' @param id The module id
#' @param initial_steps prefilled workflow steps
#'
#' @return A Shiny module UI function
#' @rdname INTERNAL_interface_module_workflow_config
#' @keywords internal
#'
#' @importFrom shiny uiOutput actionButton tagList icon
#' @importFrom htmltools div tags
#'
#' @keywords internal
interface_module_workflow_config_tab <- function(id, initial_steps) {
    doc_section_header <- function(label, href) {
        tags$h3(
            class = "workflow-doc-header",
            tags$a(
                class = "workflow-doc-link",
                href = href,
                target = "_blank",
                rel = "noopener noreferrer",
                label,
                icon("link", class = "workflow-doc-link-icon", `aria-hidden` = "true")
            )
        )
    }

    tagList(
        fluidRow(
            column(
                width = 6,
                box(
                    title = "QFeaturesGUI Workflow",
                    width = 12,
                    solidHeader = TRUE,
                    status = "primary",
                    tags$p(
                        "Start by configuring your workflow.",
                        "Steps are executed sequentially, and each step must be saved",
                        "before the next one becomes available."
                    ),
                    tags$h3("Suggested Workflow"),
                    tags$ol(
                        tags$li("Filtering NAs by Features"),
                        tags$li("Filtering NAs by Samples"),
                        tags$li("Sample Filtering"),
                        tags$li("Feature Filtering"),
                        tags$li("Normalisation"),
                        tags$li("Aggregation"),
                        tags$li("Join"),
                        tags$li("Aggregation")
                    ),
                    tags$hr(),
                    doc_section_header(
                        "Sample Filtering",
                        "https://uclouvain-cbio.github.io/QFeaturesGUI/articles/processQFeatures.html#sample-filtering"
                    ),
                    tags$p(
                        "Filter your QFeatures object using sample-level metadata",
                        "(from colData). You can combine multiple conditions",
                        "across one or more metadata fields."
                    ),
                    tags$p(
                        "Once filtering is applied, use the PCA preview to inspect",
                        "the result. When you are satisfied, click \"Save set\"."
                    ),
                    doc_section_header(
                        "Feature Filtering",
                        "https://uclouvain-cbio.github.io/QFeaturesGUI/articles/processQFeatures.html#features-filtering"
                    ),
                    tags$p(
                        "Filter your QFeatures object using feature-level metadata",
                        "(rowData). Combine one or more conditions to keep",
                        "features relevant for downstream analysis."
                    ),
                    tags$p(
                        "Review the result and save the set when the filtering",
                        "matches your analysis goals."
                    ),
                    doc_section_header(
                        "Normalisation",
                        "https://uclouvain-cbio.github.io/QFeaturesGUI/articles/processQFeatures.html#normalisation"
                    ),
                    tags$p(
                        "Apply one of several normalisation methods to the selected",
                        "sets."
                    ),
                    tags$p(
                        "Use the pre/post density plots to compare distribution",
                        "changes and select the most appropriate method."
                    ),
                    doc_section_header(
                        "Filtering Missing Values by Samples",
                        "https://uclouvain-cbio.github.io/QFeaturesGUI/articles/processQFeatures.html#filtering-missing-values"
                    ),
                    tags$p(
                        "Define a missing-value threshold and remove samples that",
                        "exceed it."
                    ),
                    tags$p(
                        "The missing-value distribution plot helps you choose a",
                        "threshold that balances quality and sample retention."
                    ),
                    doc_section_header(
                        "Filtering Missing Values by Features",
                        "https://uclouvain-cbio.github.io/QFeaturesGUI/articles/processQFeatures.html#filtering-missing-values"
                    ),
                    tags$p(
                        "This step follows the same logic as sample-level missing",
                        "value filtering, but applies it to features."
                    ),
                    doc_section_header(
                        "Aggregation",
                        "https://uclouvain-cbio.github.io/QFeaturesGUI/articles/processQFeatures.html#aggregation"
                    ),
                    tags$p(
                        "Aggregate lower-level features to a higher level.",
                        "For example, PSM-level sets can be aggregated into",
                        "peptide-level sets."
                    ),
                    doc_section_header(
                        "Join",
                        "https://uclouvain-cbio.github.io/QFeaturesGUI/articles/processQFeatures.html#join"
                    ),
                    tags$p(
                        "Join multiple sets into a single set for downstream",
                        "analysis or export."
                    ),
                    collapsible = FALSE
                )
            ),
            column(
                width = 6,
                box(
                    title = "Workflow Configuration",
                    width = 12,
                    solidHeader = FALSE,
                    status = "primary",
                    tagList(
                        fluidRow(
                            column(
                                5,
                                div(
                                    class = "panel",
                                    tags$h4("Available steps"),
                                    div(
                                        id = NS(id, "palette"),
                                        class = "palette-container",
                                        lapply(
                                            c(
                                                "Sample Filtering",
                                                "Feature Filtering",
                                                "Normalisation",
                                                "Filtering NAs by Features",
                                                "Filtering NAs by Samples",
                                                "Aggregation",
                                                "Join"
                                            ),
                                            function(s) div(class = "step", `data-step` = s, s)
                                        )
                                    )
                                )
                            ),
                            column(
                                7,
                                div(
                                    class = "panel",
                                    tags$h4("Workflow"),
                                    div(
                                        id = NS(id, "workflow"),
                                        class = "workflow-drop workflow-container",
                                        lapply(
                                            initial_steps,
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

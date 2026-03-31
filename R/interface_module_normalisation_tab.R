#' normalisation tab (section) ui builder
#'
#' @return A shiny tagList object that contains the normalisation tab UI components
#' @rdname INTERNAL_interface_module_normalisation_tab
#' @keywords internal
#'
#' @importFrom shiny fluidRow column NS actionButton icon uiOutput textOutput
#' @importFrom shinydashboardPlus box
#' @importFrom htmltools tagList h2 tags
#' @importFrom shinyBS bsTooltip
#'
interface_module_normalisation_tab <- function(id) {
    tagList(
        fluidRow(
            box(
                title = "Density Plots",
                status = "primary",
                width = 9,
                solidHeader = FALSE,
                collapsible = TRUE,
                fluidRow(
                    column(
                        6,
                        tags$h4("Pre-normalisation"),
                        waiter::withWaiter(
                            plotlyOutput(outputId = NS(id, "density_plot_pre")),
                            html = waiter::spin_fading_circles(),
                            color = "rgba(0, 0, 0, 0.25)"
                        )
                    ),
                    column(
                        6,
                        tags$h4("Post-normalisation"),
                        div(
                            style = "text-align: center; font-size: 16px; color: #777;",
                            textOutput(NS(id, "post_density_message"))
                        ),
                        waiter::withWaiter(
                            plotlyOutput(outputId = NS(id, "density_plot_post")),
                            html = waiter::spin_fading_circles(),
                            color = "rgba(0, 0, 0, 0.25)"
                        )
                    )
                )
            ),
            box(
                title = "Settings",
                status = "primary",
                width = 3,
                solidHeader = TRUE,
                collapsible = TRUE,
                selectInput(
                    inputId = NS(id, "method"),
                    label = "method",
                    choices = c(
                        "sum",
                        "max",
                        "center.mean",
                        "center.median",
                        "div.mean",
                        "div.median",
                        "diff.median",
                        "quantiles",
                        "quantiles.robust",
                        "vsn"
                    ),
                    selected = "center.median"
                ),
                br(),
                actionButton(
                    inputId = NS(id, "apply_normalisation"),
                    label = "Apply normalisation",
                    width = "100%",
                    class = "load-button"
                ),
                br(), br(),
                tags$h4("Plot options"),
                selectInput(
                    inputId = NS(id, "color"),
                    label = "Color by",
                    choices = c("NULL"),
                    selected = "NULL"
                )
            )
        ),
        actionButton(
            NS(id, "export"),
            "Save the processed assays",
            icon("hand-pointer", class = "fa-solid"),
            width = "100%",
            class = "load-button"
        ),
        shinyBS::bsTooltip(
            id = NS(id, "export"),
            title = paste("Write the processed assays to the QFeatures object.",
                "This is needed to proceed to the next steps.",
                sep = " "
            ),
            trigger = "hover",
            placement = "top"
        )
    )
}

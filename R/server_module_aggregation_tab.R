#' Server for the module aggregation tab
#'
#' @param id module id
#' @return The server logic for the aggregation tab
#' @rdname INTERNAL_server_module_aggregation_tab
#' @keywords internal
#'
#' @importFrom shiny moduleServer updateSelectInput observeEvent eventReactive is.reactive
#' @importFrom MultiAssayExperiment getWithColData
#'
server_module_aggregation_tab <- function(id, step_number, step_rv, parent_rv) {
  moduleServer(id, function(input, output, session) {
    pattern <- paste0("_(QFeaturesGUI#", step_number - 1, ")")
    step_ready <- reactive({
      if (!is.null(parent_rv)) req(parent_rv() > 0L)
      TRUE
    })
    
    parent_assays <- reactive({
      req(step_ready())
      error_handler(page_assays_subset,
          component_name = "Page assays subset",
          qfeatures = .qf$qfeatures,
          pattern = pattern
      )
    })
    
    aggregation_names <- reactive({
      req(parent_assays())
      annotation_cols(parent_assays(), "rowData")
    })
    
    observe({
      updateSelectInput(
        inputId = "fcol",
        choices = as.character(aggregation_names())
      )
    })
    
    unique_features <- reactive({
      req(parent_assays())
      req(input$fcol)
      unique(rbindRowData(parent_assays(), seq_along(parent_assays))[[input$fcol]])
    })
    
    observe({
      req(unique_features())
      updateSelectizeInput(
        inputId = "features",
        choices = unique_features(),
        server = TRUE
      )
    })
    
    observe({
      req(parent_assays())
      updateSelectInput(session,
                        "color",
                        choices = c("NULL",colnames(colData(parent_assays())))
      )
    })
    clicked <- reactiveVal(FALSE)
    observeEvent(input$aggregate, {
      clicked(TRUE)
    })

    output$aggregation_boxplot_ui <- renderUI({
      if (!clicked()) {
        return(NULL)
      }
      interface_module_feature_levels_boxplot(
        NS(id, "aggregation_boxplot")
      )
    })

    output$pre_boxplot <- renderText({
      if(!clicked()) {
        "The graph will be displayed once you have done the aggregation."
      } else {
        
      }
    })
    
    
    processed_assays <- eventReactive(input$aggregate, {
      req(parent_assays())
      req(input$fcol)
      error_handler(
        aggregation_qfeatures,
        component_name = "aggregation",
        qfeatures = parent_assays(),
        method = as.character(input$method),
        fcol = input$fcol
      )
    })
    
    #TO DO 
    # extract server module from observer
    observe({
      req(processed_assays())
      req(input$color)
      req(input$features)
      server_module_boxplot_box(
        id = "aggregation_boxplot",
        qf = parent_assays(),
        qf_aggregate = processed_assays(),
        aggregateBy = input$fcol,
        feature = input$features,
        color = input$color,
        showPoints = reactive(input$addPoints)
      )
    })
    
    
    observeEvent(input$export, {
      req(processed_assays())
      with_task_loader(
        caption = "Saving sets in QFeatures object",
        expr = {
          error_handler(
            add_assays_to_global_rv,
            component_name = "Add assays to global_rv",
            processed_qfeatures = processed_assays(),
            step_number = step_number,
            type = "aggregation",
            varTo = input$fcol,
            varFrom = input$fcol
          )
          global_rv$codeLines[[paste0("Initialization_names_", step_number)]] <- codeGeneratorInitialization(qf = .qf$qfeatures, step_number = step_number)
          global_rv$codeLines[[paste0("aggregagtion_", step_number)]] <- codeGeneratorAggregation(method = as.character(input$method), fcol = input$fcol, step_number = step_number)
          step_rv(step_rv() + 1L)
        }
      )
    })
  })
}

#' Server for the module boxplot box
#'
#' @param id module id
#' @param qf qfeatures object before aggregation
#' @param qf_aggregate qfeatures object after aggregation
#' @param aggregateBy data to aggregate QFeatures object on
#' @param feature feature to plot
#' @param color variable to use for the color
#' @param showPoints logical for hide/show points on boxplot
#' @return a boxplot
#' @rdname INTERNAL_server_module_boxplot_box
#' @keywords internal
#'
#' @importFrom shiny moduleServer updateSelectInput observeEvent eventReactive is.reactive
#' @importFrom dplyr mutate
#' @importFrom tibble rownames_to_column
#' @importFrom QFeatures aggregateFeatures
#' @importFrom stats na.exclude
#' @importFrom tidyr gather
#'

server_module_boxplot_box <- function(id, qf, qf_aggregate, aggregateBy, feature, color, showPoints) {
  moduleServer(id, function(input, output, session){
    df_qf_list <- list()
    df_qf_aggregate_list <- list()
    for(i in names(qf)){
      set_qf <- qf[[i]][rowData(qf[[i]])[[aggregateBy]] == feature,]
      set_qf_aggregate <- qf_aggregate[[i]][rowData(qf_aggregate[[i]])[[aggregateBy]] == feature,]
      if (color == "NULL"){
        df_qf_list[[i]] <- assay(set_qf) |>
          as.data.frame() |>
          rownames_to_column(var = "aggregation") |>
          tidyr::gather(sample,intensity, -aggregation) |>
          na.exclude()
        df_qf_aggregate_list[[i]] <- assay(set_qf_aggregate) |>
          as.data.frame()|>
          rownames_to_column(var = "aggregation") |>
          tidyr::gather(sample,intensity,-aggregation) |>
          na.exclude()
      } else {
        df_qf_list[[i]] <- assay(set_qf) |>
          as.data.frame() |>
          rownames_to_column(var = "aggregation") |>
          tidyr::gather(sample,intensity, -aggregation) |>
          mutate(condition = as.vector(colData(qf)[sample, color])) |>
          na.exclude()
        df_qf_aggregate_list[[i]] <- assay(set_qf_aggregate) |>
          as.data.frame()|>
          rownames_to_column(var = "aggregation") |>
          tidyr::gather(sample,intensity,-aggregation) |>
          mutate(condition = as.vector(colData(qf_aggregate)[sample, color])) |>
          na.exclude()
      }
    }
    
    df_qf <- dplyr::bind_rows(df_qf_list)
    df_qf_aggregate <- dplyr::bind_rows(df_qf_aggregate_list)
    data_final <- dplyr::bind_rows(df_qf,df_qf_aggregate)
    
    data_final$aggregation <- factor(data_final$aggregation, levels = unique(data_final$aggregation))
    
    output$boxplot <- renderPlotly({
      error_handler(
        create_boxplot,
        component_name = "boxplot generation",
        data = data_final,
        color = color,
        showPoints = showPoints()
      )
    }) 
  }) 
}

#' Function that generate a boxplot
#'
#' @param data a dataset containing intensity values pre and post aggregation
#' @param color variable to use for the color
#' @param showPoints logical for hide/show points on boxplot
#' @return a boxplot
#' @rdname INTERNAL_create_boxplot
#' @keywords internal
#'
#' @importFrom plotly plotly_build
#'

create_boxplot<- function(data, color, showPoints){
  if(showPoints) {
    boxpoints = "all"
    jitter = 0.3
    pointpos = 0
  } else {
    boxpoints = "suspectedoutliers"
    jitter = 0
    pointpos = 0
  }
  p <-plot_ly(
    data = data,
    x = ~aggregation,
    y = ~intensity,
    color = if (color == "NULL") NULL else ~condition,
    text = ~sample,
    type = "box",
    boxpoints = boxpoints,
    jitter = jitter,
    pointpos = pointpos,
    hoveron = if(showPoints) "points" else "boxes",
    hovertemplate = paste0(
      "<b>%{text}</b><br>",
      "<extra></extra>"
    )
  ) %>%
    layout(boxmode = "group")
  plotly_build(p)
}

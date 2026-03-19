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
    
    observe({
      updateSelectInput(
        inputId = "fun"
      )
    })
    ### pass unique features into utils.r
    unique_features <- reactive({
      req(parent_assays())
      req(input$fcol)
      features_list <- lapply(seq_along(parent_assays()), function(i) {
        rowData(parent_assays()[[i]])[, input$fcol]
      })
      features_vector <- unlist(features_list)
      unique(features_vector)
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
                        choices = colnames(colData(parent_assays()))
      )
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
    
    
    # output$density_plot <- renderPlotly({
    #   req(processed_assays())
    #   density_by_sample_plotly(
    #     qfeatures = processed_assays(),
    #     color = input$color
    #   )
    # })
    observe({
      req(processed_assays())
      req(input$color)
      req(input$fcol)
      req(input$features)
      server_module_boxplot_box(
        id = "aggregation_boxplot",
        qf = parent_assays(),
        qf_aggregate = processed_assays(),
        aggregateBy = input$fcol,
        feature = input$features,
        color = input$color
      )
    })
    
    
    
    
    observeEvent(input$export, {
      req(processed_assays())
      error_handler(
        add_assays_to_global_rv,
        component_name = "Add assays to global_rv",
        processed_qfeatures = processed_assays(),
        step_number = step_number,
        type = "aggregation",
        varTo = input$fcol,
        varFrom = input$fcol
      )
    })
  })
}

#' Server for the module boxplot box
#'
#' @param id module id
#' @return The server logic for the aggregation tab
#' @rdname INTERNAL_server_module_aggregation_tab
#' @keywords internal
#'
#' @importFrom shiny moduleServer updateSelectInput observeEvent eventReactive is.reactive
#' @importFrom ggplot2 geom_boxplot geom_point ggplot theme xlab ylab
#' @importFrom dplyr mutate
#' @importFrom tibble rownames_to_column
#'

server_module_boxplot_box <- function(id, qf, qf_aggregate, aggregateBy, feature, color) {
  moduleServer(id, function(input, output, session){
    df_qf_list <- list()
    df_qf_aggregate_list <- list()
    
    for(i in names(qf)){
      set_qf <- qf[[i]][rowData(qf[[i]])[[aggregateBy]] == feature,]
      set_qf_aggregate <- qf_aggregate[[i]][rowData(qf_aggregate[[i]])[[aggregateBy]] == feature,]
      df_qf_list[[i]] <- assay(set_qf) |>
        as.data.frame() |>
        rownames_to_column(var = "aggregation") |>
        tidyr::gather(sample,intensity, -aggregation) |>
        mutate(condition = colData(qf)[sample,color]) |>
        na.exclude()
      df_qf_aggregate_list[[i]] <- assay(set_qf_aggregate) |>
        as.data.frame()|>
        rownames_to_column(var = "aggregation") |>
        tidyr::gather(sample,intensity,-aggregation) |>
        mutate(condition = colData(qf_aggregate)[sample,color]) |>
        na.exclude()
    }
    
    df_qf <- dplyr::bind_rows(df_qf_list)
    df_qf_aggregate <- dplyr::bind_rows(df_qf_aggregate_list)
    data_final <- dplyr::bind_rows(df_qf,df_qf_aggregate)
    
    data_final$aggregation <- factor(data_final$aggregation, levels = unique(data_final$aggregation))
    
    output$boxplot <- renderPlot({
      ggplot(data_final, aes(x = aggregation, y = intensity))+geom_boxplot(aes(colour = condition)) +
        geom_point(position = position_dodge(0.75), aes(colour = condition))+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
        xlab("Features name")+
        ylab("Intensity (log2)")
      
    })
  }) 
}
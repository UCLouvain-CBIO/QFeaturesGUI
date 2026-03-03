server_module_missing_values_tab <- function(id, step_number, type){
  moduleServer(id, function(input, output, session) {
    assays_to_process <- eventReactive(input$reload, {
      error_handler(page_assays_subset,
                    component_name = "Page assays subset",
                    qfeatures = global_rv$qfeatures,
                    pattern = paste0("_(QFeaturesGUI#", step_number - 1, ")")
      )
    })
    filteringTable <- reactive({
      req(assays_to_process())
      tableNA <- QFeatures::nNA(
        object = assays_to_process(),
        i = seq_along(assays_to_process())
      )
      if(type == "features"){
        filteringTable <- tableNA$nNAcols
      } else {
        filteringTable <- tableNA$nNArows
      }
      updateSelectInput(
        session = session,
        inputId = paste0("pca_color_", type),
        choices = colnames(filteringTable)
      )
      
      filteringTable
    })
    observe({
      req(filteringTable())
      output[[paste0("dynamic_",type)]] <- renderUI({
        if (length(unique(filteringTable()$name)) > 10) {
          plotOutput(NS(id,paste0("plot_na_",type)))
        } else {
          DT::dataTableOutput(NS(id, paste0("dataTable_na_", type)))
        } 
      })
      output[[paste0("dataTable_na_",type)]] <- DT::renderDataTable({
        DT::datatable(
          as.data.frame(filteringTable()),
          options = list(scrollX = TRUE))
      })
      output[[paste0("plot_na_", type)]] <- renderPlot({
        ggplot(filteringTable())+
          geom_histogram(
            aes(
              x=pNA,
              fill = .data[[input[[paste0("pca_color_", type)]]]]
            ),
            show.legend = input[[paste0("show_legend_", type)]]
          )+
          geom_vline(
            xintercept = input[[paste0("threshold_", type)]],
            colour = "red"
          )+
          annotate(
            "rect",
            xmin = input[[paste0("threshold_", type)]],
            xmax = Inf,
            ymin = -Inf,
            ymax = Inf,
            alpha = .5
          )
      })
      output[[paste0("nb_removed_", type)]] <- renderInfoBox({
        nbRemoved <- sum(filteringTable()$pNA > input[[paste0("threshold_", type)]])
        infoBox(paste0("Number of ", type, " removed: "), nbRemoved, fill = TRUE, color = "light-blue", icon = icon("filter"))
      })
      output[[paste0("percent_removed_", type)]] <- renderInfoBox({
        nbRemoved <- sum(filteringTable()$pNA > input[[paste0("threshold_", type)]])
        percent <- nbRemoved/length(filteringTable()$pNA)*100
        infoBox(paste0("Percent of ", type, " removed: "), paste(percent,"%"), fill = TRUE, color = "light-blue", icon = icon("percent"))
      })
    })

    observeEvent(input$export, {
      req(filteringTable())
      shinycssloaders::showPageSpinner(
        type = "6",
        caption = "The filtering of QFeatures object can be quite time consuming for large datasets"
      )
      processed_assays <- QFeatures::filterNA(assays_to_process(),
                                   i = seq_along(assays_to_process()), 
                                   pNA = input[[paste0("threshold_", type)]])
      error_handler(
        add_assays_to_global_rv,
        component_name = "Add assays to global_rv",
        processed_qfeatures = processed_assays,
        step_number = step_number,
        type = "features_filtering"
      )
      shinycssloaders::hidePageSpinner()
    })
  })
}
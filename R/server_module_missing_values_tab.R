server_module_missing_values_tab <- function(id, step_number, type){
  moduleServer(id, function(input, output, session) {
    assays_to_process <- eventReactive(input$reload, {
      error_handler(page_assays_subset,
                    component_name = "Page assays subset",
                    qfeatures = global_rv$qfeatures,
                    pattern = paste0("_(QFeaturesGUI#", step_number - 1, ")")
      )
    })
    observe({
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
      output[[paste0("plot_na_", type)]] <- renderPlot({
        ggplot(filteringTable)+
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
        nbRemoved <- sum(filteringTable$pNA > input[[paste0("threshold_", type)]])
        infoBox(paste0("Number of ", type, " removed: "), nbRemoved, fill = TRUE, color = "light-blue")
      })
      output[[paste0("percent_removed_", type)]] <- renderInfoBox({
        nbRemoved <- sum(filteringTable$pNA > input[[paste0("threshold_", type)]])
        percent <- nbRemoved/length(filteringTable$pNA)
        if(percent < 0.25){color <- "olive"}else if(percent >=0.25 && percent<0.75){color <- "orange"}else{color <- "red"}
        infoBox(paste0("Percent of ", type, " removed: "), percent, fill = TRUE, color = color)
      })
    })
  })
}
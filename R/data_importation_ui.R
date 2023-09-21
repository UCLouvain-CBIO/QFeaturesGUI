data_importation_tab <- tabPanel(
  "Data Importation",

  fluidPage(

    column(6,
           wellPanel(
             h3("Input Table"),

             fluidRow(
               column(7,fileInput("input_table", NULL, accept = c(".csv", ".tsv"))),

               column(5, actionButton("input_settings", "Import settings",
                                      width = "100%",
                                      style= "color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                      icon = icon("glyphicon glyphicon-cog", lib ='glyphicon')))
             ),
             DT::dataTableOutput("input_df_table")
           ),


           wellPanel(
             h3("Sample Table"),
             fluidRow(
               column(7, fileInput("sample_table", NULL, accept = c(".csv", ".tsv"))),

               column(5, actionButton("sample_settings", "Import settings",
                                      width = "100%",
                                      style= "color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                      icon = icon("glyphicon glyphicon-cog", lib ='glyphicon')))
             ),
             DT::dataTableOutput("sample_df_table")
           )
        ),


    column(6,
           wellPanel(
             h3("QFeatures Converter"),
             selectInput("batch_col", "Choose the approriate batch column :", choices = NULL),
             selectInput("channel_col", "Choose the approriate channel column :", choices = NULL),
             checkboxInput("removeEmptyCols", label = "Remove in each batch the columns that contain only missing values", value = FALSE),
             actionButton("convert", "Convert to a QFeatures object"),
             downloadButton("download_qfeat",
                            "Download the created QFeatures object as a .rds File"),
             br(),
             br(),
             shinyBS::bsAlert("convert_error"),
             shinyBS::bsAlert("convert_warning"),
             h3("QFeatures Preview"),
             DT::dataTableOutput("qfeat_table"),
             h3("Selected Assay"),
             DT::dataTableOutput("assay_table")
           ))
  )
)

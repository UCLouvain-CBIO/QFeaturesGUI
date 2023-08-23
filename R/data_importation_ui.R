library(shiny)
# make reusable ui components with functions
# same for server modules

data_importation_tab <- tabPanel(
  "Data Importation",

  fluidPage(
    column(6,
           wellPanel(
             h3("Input Table"),
             fileInput("input_table", NULL, accept = c(".csv", ".tsv")),
             DT::dataTableOutput("input_df_table")
           ),
           wellPanel(
             h3("Sample Table"),
             fileInput("sample_table", NULL, accept = c(".csv", ".tsv")),
             DT::dataTableOutput("sample_df_table")
           )
        )
           ,
    column(6,
           wellPanel(
             h3("QFeatures Converter"),
             selectInput("batch_col", "Choose the approriate batch column :", choices = NULL ),
             selectInput("channel_col", "Choose the approriate channel column :", choices = NULL),
             actionButton("convert", "Convert to a QFeatures object"),
             downloadButton("download_qfeat",
                            "Download the created QFeatures object as a .rds File")
           ))
  )
)

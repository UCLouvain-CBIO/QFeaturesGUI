# make reusable ui components with functions
# same for server modules

data_importation_tab <- tabPanel(
  "Data Importation",

  fluidPage(
    column(6,
           wellPanel(
             h3("Input Table"),

             fluidRow(
               column(8, fileInput("input_table", NULL, accept = c(".csv", ".tsv"))),

               column(3, actionButton("input_settings", "Import settings"), actionButton("import_input", "Import"))
             ),

             shinyBS::bsModal("modal_input_settings", "Importation settings", "input_settings", size = "large",
                      textInput("input_sep", label = "Insert the separator character used", value = ","),
                      textInput("input_dec", label = "Insert the decimal character used", value = "."),
                      textInput("input_com", label = "Insert the comment character used", value = "#"),
                      numericInput("input_skip", label = "Insert the number of line to skip before beginning to read data ", value = 0),
                      checkboxInput("input_factor", label = "String as Factor", value = FALSE)
                     ),

             DT::dataTableOutput("input_df_table")
           ),


           wellPanel(
             h3("Sample Table"),
             fluidRow(
               column(8, fileInput("sample_table", NULL, accept = c(".csv", ".tsv"))),

               column(3, actionButton("sample_settings", "Import settings"), actionButton("import_sample", "Import"))
             ),

             shinyBS::bsModal("modal_sample_settings", "Importation settings", "sample_settings", size = "large",
                              textInput("sample_sep", label = "Insert the separator character used", value = ","),
                              textInput("sample_dec", label = "Insert the decimal character used", value = "."),
                              textInput("sample_com", label = "Insert the comment character used", value = "#"),
                              numericInput("sample_skip", label = "Insert the number of line to skip before beginning to read data ", value = 0),
                              checkboxInput("sample_factor", label = "String as Factor", value = FALSE)
             ),
             DT::dataTableOutput("sample_df_table")
           )
        ),


    column(6,
           wellPanel(
             h3("QFeatures Converter"),
             selectInput("batch_col", "Choose the approriate batch column :", choices = NULL ),
             selectInput("channel_col", "Choose the approriate channel column :", choices = NULL),
             # textInput("suffix", label = "A character giving the suffix of the column names in each assay", value = NULL),
             textInput("sep", label = "A character inserted between the assay name and the suffix", value = ""),
             checkboxInput("removeEmptyCols", label = "Remove in each batch the columns that contain only missing values", value = FALSE),
             actionButton("convert", "Convert to a QFeatures object"),
             downloadButton("download_qfeat",
                            "Download the created QFeatures object as a .rds File"),
             bsAlert("convert_error"),
             bsAlert("convert_warning")
             
           ))
  )
)

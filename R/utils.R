# Convert csv or tsv to a dataframe
file_to_df<- function(file_upload){
  req(file_upload)
  ext <- tools::file_ext(file_upload$name)
  switch(ext,
         csv = read.table(file_upload$datapath, header = TRUE, sep = ",", row.names = 1),
         tsv = read.table(file_upload$datapath, header = TRUE, sep = "\t", row.names = 1),
         validate("Invalid file; Please upload a .csv or .tsv file")
  )
}

# convert a dataframe into a DTable

# df_to_DT <- function(input, df){
#   DT::renderDataTable(
#     {
#       req(input)
#       DT::datatable(df, extensions = 'FixedColumns',
#                     options = list(
#                       searching = FALSE,
#                       scrollX = TRUE,
#                       fixedColumns = TRUE)
#       )
#     }
#   )
# }

readSCP_wraper <- function(sample_table,
                           input_table,
                           featureData,
                           colData,
                           batch_col,
                           channel_col){
  tryCatch(

    {
      req(sample_table, input_table)
      readSCP(
        featureData = featureData,
        colData = colData,
        batchCol = batch_col,
        channelCol = channel_col
      )
      showNotification("Convertion succed !")
    },
    error = function(err) {
      showNotification(paste0("An error occured : ", err),duration = 10)
    },
    warning=function(war) {
      showNotification(paste0("A warning occured : ", war), duration = 10)
    }
  )
}

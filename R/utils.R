# Convert csv or tsv to a dataframe
file_to_df<- function(file_upload,
                      sep,
                      dec,
                      skip,
                      stringsAsFactors,
                      comment.char
                      ){
  tryCatch(

    {
    req(file_upload)
    table <- read.table(file_upload$datapath,
               sep = sep,
               dec = dec,
               skip = skip,
               stringsAsFactors = stringsAsFactors,
               comment.char = comment.char,
               header = TRUE,
               row.names = 1)
    showNotification("File import succed !")
    return(table)
    },
    error = function(err) {
      showNotification(paste0("An error occured during the import: ", err),duration = 10)
    },
    warning=function(war) {
      showNotification(paste0("A warning occured during the import : ", war), duration = 10)
    }
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

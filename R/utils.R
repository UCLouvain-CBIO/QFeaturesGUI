loading <- function(){
  showModal(modalDialog(
    title = "Loading",
    div(class = "progress",
        div(class = "progress-bar progress-bar-striped active", role = "progressbar",
            style = "width: 100%;")
    ),
    "Be aware that this operation can be quite time consuming for large data sets"
  ))
}


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
    loading()
    table <- read.table(file_upload$datapath,
               sep = sep,
               dec = dec,
               skip = skip,
               stringsAsFactors = stringsAsFactors,
               comment.char = comment.char,
               header = TRUE,
               row.names = 1)
    showNotification("File import succeed !")
    removeModal()
    return(table)
    },
    error = function(err) {
      showNotification(paste0("An error occurred during the import: ", err),duration = 10)
    },
    warning=function(war) {
      showNotification(paste0("A warning occurred during the import : ", war), duration = 10)
    }
  )
}

readSCP_wraper <- function(sample_table,
                           input_table,
                           featureData,
                           colData,
                           batch_col,
                           channel_col,
                           removeEmptyCols,
                           session){
  tryCatch(

    {
      req(sample_table, input_table)
      qfeat <- readSCP(
        featureData = featureData,
        colData = colData,
        batchCol = batch_col,
        channelCol = channel_col,
        removeEmptyCols = removeEmptyCols
      )
      showNotification("Conversion succeed !")
      return(qfeat)
    },
    error = function(err) {
      shinyBS::createAlert(session, "convert_error", "convert_err_alert", title = "Error while converting",
                  content = paste0(err), append = FALSE)
    },
    warning=function(war) {
      shinyBS::createAlert(session, "convert_warning", "convert_war_alert", title = "Warning(s) while converting",
                  content = paste0(war), append = FALSE)
    }
  )
}

qfeat_to_df <- function(qfeat){
  df <- data.frame(
    "Name" = rep.int(0, length(qfeat)),
    "Class" = rep.int(0, length(qfeat)),
    "nrows" = rep.int(0, length(qfeat)),
    "ncols" = rep.int(0, length(qfeat))
  )
  for (i in 1:length(qfeat)){
    print(names(qfeat)[[i]])
    df[i, "Name"] <- names(qfeat)[[i]]
    df[i, "Class"] <- class(qfeat[[i]])[[1]]
    df[i, "nrows"] <- nrow(qfeat[[i]])[[1]]
    df[i, "ncols"] <- ncol(qfeat[[i]])[[1]]
  }

  return(df)
}

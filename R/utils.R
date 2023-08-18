file_to_df<- function(file_upload){
  req(file_upload)
  ext <- tools::file_ext(file_upload$name)
  switch(ext,
         csv = read.table(file_upload$datapath, header = TRUE, sep = ",", row.names = 1),
         tsv = read.table(file_upload$datapath, header = TRUE, sep = "\t", row.names = 1),
         validate("Invalid file; Please upload a .csv or .tsv file")
  )
}

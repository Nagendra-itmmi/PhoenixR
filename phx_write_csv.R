#' @export

phx_write_csv = function(import_df, file_path)
{
  # DESCRIPTION: 
  #   A function to write a csv from a dataframe in the proper format to be uploaded into Phoenix 2.0.
  # 
  # INPUT:
  #   import_df- dataframe, the datframe to be saved as a csv.
  #   file_path- string, the path where the csv is to be saved.
  #   
  # OUTPUT: 
  #   Success:
  #     file_path- string, the path where the csv was saved.
  #   Failure:
  #     save_file- try-error, a message explaining a specific error. 
  #   
  # FUNCTION BODY:
  
  for (i in 1:ncol(import_df))
  {
    import_df[[i]] = as.character(import_df[[i]])
  }
  
  if (nrow(import_df) != 0)
  {
    import_df[is.na(import_df)] = "NULL"
    import_df[import_df == "NA"] = "NULL"
    import_df[import_df == ""] = "NULL"
    import_df[import_df == " "] = "NULL"
  }
  
  import_df = try(data.frame(lapply(import_df, function(x) {gsub(",", ";", x)})), silent = TRUE)
  
  import_df = try(data.frame(lapply(import_df, function(x) {gsub("\"", "", x)})), silent = TRUE)
  
  import_df = try(data.frame(lapply(import_df, function(x) {gsub("[\r\n]", "", x)})), silent = TRUE)

  save_file = try(write.csv(import_df, file_path, row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8"), silent = TRUE)
  
  if (class(save_file)[1] != "try-error")
  {
    return(file_path)
  }
  
  else
  {
    return(save_file)
  }
}

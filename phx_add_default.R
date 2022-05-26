#' @export

phx_add_default = function(elements_df, table_name)
{
  for (i in 1:ncol(elements_df))
  {
    elements_df[[i]] = as.character(elements_df[[i]])
  }
  
  table_name = toupper(table_name)
  id_col_name = paste0(table_name, "_ID")
  id_col_location = as.numeric(which(colnames(elements_df) == id_col_name))
  
  elements_df[[id_col_location]] = replicate(nrow(elements_df), uuid::UUIDgenerate())
  elements_df[[id_col_location]] = as.character(elements_df[[id_col_location]])
  
  date_col_location = as.numeric(which(colnames(elements_df) == "DATE_CREATED"))
  
  elements_df[[date_col_location]] = rep(as.character(format(Sys.time(), tz = "America/New_York"))
                                         , nrow(elements_df))
  elements_df[[date_col_location]] = as.character(elements_df[[date_col_location]])
  
  return(elements_df)
}

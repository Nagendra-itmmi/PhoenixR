#' @export

phx_check_insert = function(key, table_name, file_path, usr)
{
  # DESCRIPTION: 
  #   A function to insert elements into a table in Phoenix 2.0. 
  # 
  # INPUTS: 
  #   key- environment, a point of access to Phoenix 2.0 (see function in phx_login). 
  #   table_name- string, the name of the table where the elements are to be inserted. 
  #   new_elements- dataframe, that has already been mapped to match the table where 
  #   the elements are being inserted. 
  #   file_path- string, the path where the new file will be saved. 
  #   usr- the username of a Mitchell Martin employee
  # 
  # OUTPUT: 
  #   element_status- dataframe, composed of each element’s id and the status of its insert. 
  # 
  # FUNCTION BODY:
  
  file_path = gsub("//", "/", file_path)
  file_path = gsub("\\", "/", file_path, fixed = TRUE)
  
  temp = phx_write(key, paste0("PUT 'file://", file_path, "*' @%", table_name), usr)
  
  if (class(temp)[1] != "try-error")
  {
    temp = phx_write(key, paste0("COPY INTO ", table_name, " VALIDATION_MODE = RETURN_ALL_ERRORS;"), usr)
    
    if (class(temp)[1] != "try-error")
    {
      #find insert failures
      row_count = temp$rowcount
      error = c()
      if (row_count != 0)
      {
        error = temp$fetchall()
        error = unlist(error)[seq(12,(row_count*12),12)]
        error = strsplit(error, ",")
        field_count = error[[1]] %>% length()
        error = unlist(error)[seq(1, row_count*field_count, field_count)]
      }
      #
      import_file = phx_read_csv(file_path)
      temp = data.frame(element_id = import_file[[1]]
                        , element_success = (import_file[[1]] %in% error == FALSE))
      #
      phx_write(key, paste0("COPY INTO ", table_name, " ON_ERROR = CONTINUE PURGE = TRUE;"), usr)
      phx_write(key, paste0("REMOVE @%", table_name, ";"),usr)
    }
  }
  return(temp)
}

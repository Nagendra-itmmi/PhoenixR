#' @export

phx_attributes = function(key, table_name, usr)
{
  # DESCRIPTION: 
  #   A function that returns all of the attributes of a table. 
  # 
  # INPUTS: 
  #   key- environment, a point of access to Phoenix 2.0 (see function phx_login). 
  #   table_name- string, the name of the table that the attributes are being pulled from. 
  #   usr- the username of a Mitchell Martin employee
  # 
  # OUTPUT: 
  #   Success: 
  #   table_attributes- dataframe, attribute names, types, and constraints within the given table. 
  #   Failure: 
  #   table_attributes- try-error, a message specifying an error. 
  # 
  # FUNCTION BODY:
  
  table_attributes = phx_read(key, paste0("SHOW COLUMNS IN TABLE ", table_name), usr)
  
  if (class(table_attributes)[1] != "try-error")
  {
    table_attributes = table_attributes$column_name
  }

  return(table_attributes)
}
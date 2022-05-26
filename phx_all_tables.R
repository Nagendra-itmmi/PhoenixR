#' @export

phx_all_tables = function(key, usr)
{
  # DESCRIPTION: 
  #   A function that shows the user all of the tables in Phoenix 2.0. 
  # 
  # INPUTS: 
  #   key- environment, a point of access to Phoenix 2.0 (see function phx_login). 
  #   usr- the username of a Mitchell Martin employee
  # 
  # OUTPUT: 
  #   Success: 
  #   tables- string, a list of the names of all of the tables in Phoenix 2.0. 
  # Failure: 
  #   tables- try-error, a message specifying an error. 
  # 
  # FUNCTION BODY:
  
  tables = phx_read(key, "SHOW TABLES IN DATABASE", usr)
  
  if (class(tables)[1] != "try-error")
  {
    tables = as.character(tables$name)
  }
  
  return(tables)
}

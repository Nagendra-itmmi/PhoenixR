#' @export

phx_write = function(key, sql_query, usr)
{
  # DESCRIPTION: 
  #   A function to perform an SQL query that writes into Phoenix 2.0.
  # 
  # INPUTS: 
  #   key- environment, a point of access to Phoenix 2.0 (see function in phx_login). 
  #   sql_query- string, a raw SQL query. 
  #   usr- the username of a Mitchell Martin employee
  # 
  # OUTPUT: 
  #   Success: 
  #   query_result- string, the SQL query that was executed in Phoenix 2.0.  
  #   Failure: 
  #   query_result- try-error, a message specifying an error. 
  #   
  # FUNCTION BODY:
  
  if (stringr::str_sub(sql_query, 1, 6) != "select"
      && stringr::str_sub(sql_query, 1, 6) != "SELECT"
      && stringr::str_sub(sql_query, 1, 6) != "Select"
      && stringr::str_sub(sql_query, 1, 4) != "show"
      && stringr::str_sub(sql_query, 1, 4) != "SHOW"
      && stringr::str_sub(sql_query, 1, 4) != "Show")
  {
    query_result = try(key$cursor()$execute(sql_query), silent = TRUE)
    
    sql_query = gsub("'", "*", sql_query)
    
    if (class(query_result)[1] != "try-error")
    {
      activity_log_query = paste0("INSERT INTO ACTIVITY_LOG VALUES ('"
                                  , uuid::UUIDgenerate()
                                  ,"', '"
                                  , as.character(format(Sys.time(), tz = "America/New_York"))
                                  , "', '"
                                  , phx_get_user_id(key, usr)
                                  , "', '"
                                  , sql_query
                                  , "', "
                                  , "NULL)")
      
      activity_log = key$cursor()$execute(activity_log_query)
    }
    
    return(query_result)
  }
  
  else 
  {
    return("Error: Use the phx_read function for this type of query.")
  }
    
}

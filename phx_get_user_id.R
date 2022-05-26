#' @export

phx_get_user_id = function(key, usr)
{
  # DESCRIPTION:
  #   A function to get the Mitchell_Martin_Staff_Id of an individual based on their username.
  # 
  # INPUT:
  #   key- environment, the environment that gives access to the database. 
  #   usr- string, the username of a Mitchell Martin Staff Member 
  #   
  # OUTPUT:
  #   Success:
  #     get_id- string, the identifier of a Mitchell Martin staff member.
  #   Failure:
  #     get_id- try-error, a message specifying an error.
  #   
  # FUNCTION BODY:
  
  staff_information_query = paste0("SELECT MITCHELL_MARTIN_STAFF_ID FROM MITCHELL_MARTIN_STAFF WHERE PHOENIX_USERNAME = '"
                                   , usr
                                   , "'")
  staff_information = try(pandas$read_sql(staff_information_query, key), silent = TRUE)
  
  if (class(staff_information)[1] != "try-error")
  {
    for (i in 1:ncol(staff_information))
    {
      staff_information[[i]] = as.character(staff_information[[i]])
    }
    return(staff_information$MITCHELL_MARTIN_STAFF_ID[1])
  }
  
  else
  {
    return(staff_information)
  }
  
    
}
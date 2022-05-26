#' @export

phx_login = function(usr, snow_pwd, personal_pwd)
{
  # DESCRIPTION:
  #   A function to log into Phoenix 2.0.
  #   
  # INPUTS:
  #   usr- the username of a Mitchell Martin employee
  #   snow_pwd- the password to log into the Snowflake database
  #   personal_pwd- the personal password of a Mitchell Martin employee to log into Phoenix 2.0
  #   
  # OUTPUT:
  #   Success:
  #     key- the environment that gives access to the database
  #   Failure:
  #     key- a class-type error message
  #   
  # FUNCTION BODY:
  
  usr = tolower(usr)
  personal_pwd = tolower(personal_pwd)
  
  statement = try(snowcon$connect(
    account = 'di51742.us-east-1'
    , user = "nicolas01"
    , password = snow_pwd
    , database = 'phoenix_2_0'
    , schema = 'public'
    , warehouse = 'compute_wh'
    , role = 'sysadmin'
  ), silent = TRUE)

  if (class(statement)[1] != "try-error")
  {
    user_and_password_query = paste0("SELECT PHOENIX_USERNAME, PHOENIX_PASSWORD FROM MITCHELL_MARTIN_STAFF WHERE PHOENIX_USERNAME = "
                                     ,"'"
                                     , usr
                                     , "';"
    )
    user_and_password = try(pandas$read_sql(user_and_password_query, statement), silent = TRUE)
    
    if (is.na(user_and_password[1,1]) == FALSE 
        && user_and_password[1,2] == personal_pwd)
    {
      activity_log_query = paste0("INSERT INTO ACTIVITY_LOG VALUES ('"
                                  , uuid::UUIDgenerate()
                                  ,"', '"
                                  , as.character(format(Sys.time(), tz = "America/New_York"))
                                  , "', '"
                                  , phx_get_user_id(statement, usr)
                                  , "', '"
                                  , "Login"
                                  , "', "
                                  , "NULL)")
      activity_log = statement$cursor()$execute(activity_log_query)
      
      return(statement)
    }
    
    else 
    {
      statement = statement$close
      statement = try(snowcon$connect(account = 'di51742.us-east-1'
                                      , user = "nicolas01"
                                      , password = "123"
                                      , database = 'phoenix_2_0'
                                      , schema = 'public'
                                      , warehouse = 'compute_wh'
                                      , role = 'sysadmin')
                      , silent = TRUE)
      return(statement)
    }
    
  }
  
  else
  {
    return(statement)
  }
}

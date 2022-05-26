#' @export

phx_delete = function(key, table_name, file_path, usr)
{
  # DESCRIPTION:
  # A functions to delete elements in bulk
  #
  # INPUTS:
  # key - environment, a point of access to Phoenix 2.0
  # table_name - string, the name of the table where the elements are to be deleted from.
  # file_path - string, the path where the new file will be saved
  # usr - the username of a Mitchell Martin employee
  #
  # OUTPUT:
  # element_status - dataframe, compose of each element's id and
  # the status of the deletion.
  #
  # FUNCTION BODY:
  
  # BLANK
  exec = phx_write(sess, "DELETE FROM Temp_Delete", usr)
  
  # READ CSV
  delete_elements = phx_read_csv(file_path)
  delete_elements = data.frame(ID = delete_elements[[1]])
  
  # PUSH INTO TEMP_DELETE
  exec = phx_insert(key, "TEMP_DELETE", phx_write_csv(delete_elements, file_path), usr)
  
  # DELETE IN BULK
  exec = phx_write(key
                   , paste0("DELETE FROM ", table_name
                            , " WHERE ", table_name, "_id IN "
                            , "(SELECT Id FROM Temp_Delete)")
                   , usr)
  
  # BLANK
  exec = phx_write(key, "DELETE FROM Temp_Delete", "nrobles01")
  
  return(delete_elements)
}

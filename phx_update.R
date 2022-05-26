#' @export

phx_update = function(key, table_name, file_path, usr)
{
  # DESCRIPTION: 
  #   A function to update elements in a table in Phoenix 2.0.
  # 
  # INPUTS: 
  #   key- environment, a point of access to Phoenix 2.0 (see function in phx_login). 
  #   table_name- string, the name of the table where the elements are to be inserted. 
  #   file_path- string, the path where the new file will be saved. 
  #   usr- the username of a Mitchell Martin employee
  #   
  # OUTPUT: 
  #   element_status- dataframe, composed of each element’s id and the status of its  
  #   insert. The status can be either insert failure, update failure, or success. 
  #   
  # FUNCTION BODY:
  
  #READ CSV
  update_elements = phx_read_csv(file_path)
  
  #CREATE TEMPORARY TABLE
  samp = c(1:200)
  temp_num = sample(samp, 1)
  temp_name = paste0("TEMP", temp_num)
  
  clone_table_query = paste0("CREATE TABLE ", temp_name, " CLONE ", table_name)
  clone_table = try(phx_write(key, clone_table_query, usr), silent = TRUE)
  
  #CHANGE MAP OF CSV
  colnames(update_elements)[1] = paste0(temp_name, "_ID")
  download_new_csv = phx_write_csv(update_elements, file_path)
  
  if (class(clone_table)[1] != "try-error")
  {
    #DELETE ELEMENTS FROM CLONE TABLE
    
    delete_elements_query = paste0("DELETE FROM ", temp_name)
    
    delete_elements = phx_write(key, delete_elements_query, usr)
    
    #RENAME ID COLUMN IN CLONE TABLE
    change_column_name_query = paste0("ALTER TABLE ", temp_name, " RENAME COLUMN ", table_name, "_ID TO ", temp_name, "_ID")
    change_column_name = phx_write(key, change_column_name_query, usr)
    
    #DROP UNUSED ATTRIBUTES FROM CLONE TABLE
    
    original_attributes = phx_attributes(key, temp_name, usr)
    attributes_in_new = colnames(update_elements)
    
    unused_attributes = original_attributes[original_attributes %in% attributes_in_new == FALSE]
    unused_attributes = paste0(unused_attributes, collapse = ",")
    
    drop_attributes_query = paste0("ALTER TABLE ", temp_name, " DROP COLUMN ", unused_attributes, ";")
    
    drop_attributes = try(phx_write(key, drop_attributes_query, usr), silent = TRUE)
    
    if (class(drop_attributes)[1] != "try-error")
    {
      #INSERT NEW ELEMENTS INTO CLONE TABLE
      
      insert_clone_elements = try(phx_check_insert(key, temp_name, file_path, usr), silent = TRUE)
      
      if (class(insert_clone_elements)[1] != "try-error")
      {
        #UPDATE ORIGINAL TABLE FROM CLONE TABLE
        
        insert_fail = insert_clone_elements[insert_clone_elements$element_success == FALSE, ]
        
        change_column_name_back_query = paste0("ALTER TABLE ", temp_name, " RENAME COLUMN ", temp_name, "_ID TO ", table_name, "_ID")
        change_column_name_back = phx_write(key, change_column_name_back_query, usr)
        
        updated_attributes = phx_attributes(key, temp_name, usr)
        
        attribute_queries = c()

        attribute_queries = c(attribute_queries
                                , paste0(paste0(table_name, ".", updated_attributes), " = ", paste0(temp_name, ".", updated_attributes)))
        
        attribute_queries = paste0(attribute_queries, collapse = ",")
        
        temp_table_query = paste0("SELECT * FROM ", temp_name)
        temp_table = phx_read(key, temp_table_query, usr)
        
        update_table_query = paste0("UPDATE "
                                    , table_name
                                    , " SET "
                                    , attribute_queries
                                    , " FROM "
                                    , temp_name
                                    , " WHERE "
                                    , paste0(paste0(table_name, ".", table_name, "_ID"), " = ", paste0(temp_name, ".", table_name, "_ID")))
        
        update_table = try(phx_write(key, update_table_query, usr), silent = TRUE)
        
        #SAVE TABLES AS DATAFRAMES
        colnames(update_elements)[1] = paste0(table_name, "_ID")
        attributes_in_new = colnames(update_elements)
        
        updated_table_query = paste0("SELECT "
                                     , paste0(attributes_in_new, collapse = ", ")
                                     , " FROM "
                                     , table_name
                                     , " WHERE "
                                     , paste0(table_name, "_ID")
                                     , " IN (SELECT "
                                     , paste0(table_name, "_ID")
                                     , " FROM "
                                     , temp_name
                                     , ")")

        updated_table = phx_read(key, updated_table_query, usr)
        
        #COMBINE COLUMNS OF DATAFRAMES  
        temp_table_vec = paste0(temp_table[1:length(temp_table[1,]),])
        
        #ORDER THE TABLES BY ID
        temp_table = temp_table[order(temp_table[[1]]), ]
        updated_table = updated_table[order(updated_table[[1]]), ]
        
        #CHECK UPDATE STATUS
        
        col_query = apply(temp_table, 1, paste0, collapse = "")

        updated_query = apply(updated_table, 1, paste0, collapse = "")
        
        check_element_id = c()
        check_element_success = c()
        
        current_id = stringr::str_sub(as.character(col_query), 1, 36)
        
        check_element_id = c(check_element_id, current_id)
        check_element_success = c(check_element_success, col_query %in% updated_query)
        
        update_check = data.frame(check_element_id, check_element_success)
        colnames(update_check) = colnames(insert_fail)
        update_check = rbind(update_check, insert_fail)
        phx_write(key, paste0("DROP TABLE ", temp_name), usr)
        
        return(update_check)
      }
      
      else 
      {
        phx_write(key, paste0("DROP TABLE ", temp_name), usr)
        return(insert_clone_elements)
      }
      
    }
    
    else
    {
      phx_write(key, paste0("DROP TABLE ", temp_name), usr)
      return(drop_attributes)
    }
    
  }
  
  else
  {
    return (clone_table)
  }
  
}

#' @export

phx_complete = function(key, table_name, input_df, usr)
{
  # DESCRIPTION: 
  #   A function to add and rearrange columns of a given data frame to match with its  
  #   table in Phoenix. 
  # 
  # INPUTS: 
  #   table_name- string, the name of the table in Phoenix 2.0 the dataframe needs to  
  #   match. 
  #   input_df- dataframe, the datframe that needs to be completed.
  #   usr- the username of a Mitchell Martin employee
  #   
  # OUTPUT: 
  #   Success:  
  #   output_df- dataframe, the updated dataframe updated with all of the columns.  
  #   Failure:  
  #   output_df- try-error, a message specifying and error. 
  # 
  # FUNCTION BODY:
  
  for (i in 1:ncol(input_df))
  {
    input_df[[i]] = as.character(input_df[[i]])
  }
  
  table_columns = phx_attributes(key, table_name, usr)
  
  if (class(table_columns)[1] != "try-error")
  {
    input_df_columns = colnames(input_df)
    
    missing_columns = c()
    for (i in 1:length(table_columns))
    {
      current_column = table_columns[i]
      
      if (current_column %in% input_df_columns == FALSE)
      {
        missing_columns = c(missing_columns, current_column)
      }
      
      i = i + 1
      
    }
    
    if (length(missing_columns) > 0)
    {
      for (j in 1:length(missing_columns))
      {
        input_df = cbind(input_df, "NULL")
      }
    }
    
    colnames(input_df) = c(input_df_columns,  as.character(missing_columns[1:length(missing_columns)]))
    
    output_df = input_df[, c(table_columns)]
    
    return(output_df)
  }
  
  else
  {
    return(table_columns)
  }
  
}
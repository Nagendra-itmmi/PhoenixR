#' @export

phx_map = function(input_df, map)
{
  # DESCRIPTION: 
  #   A function to map a given data frame to its proper columns based on table from  
  # Phoenix. 
  # 
  # INPUTS: 
  #   input_df- dataframe, the dataframe to be mapped. 
  #   map- vector, composed of the original column names in the order of which they  
  #   appear in the table in Phoenix 2.0. 
  # 
  # OUTPUT: 
  #   Success: 
  #   output_df- dataframe, the new dataframe mapped to match the table. 
  # Failure: 
  #   output_df- try-error, a message specifying an error. 
  # 
  # FUNCTION BODY:
  
  for (i in 1:ncol(input_df))
  {
    input_df[[i]] = as.character(input_df[[i]])
  }
  
  if(length(map) != length(colnames(input_df)))
  {
    return("Error: Length of map does not match the number of columns.")
  }
  
  else
  {
    output_df = input_df
    colnames(output_df) = map
    return(output_df)
  }
    
}
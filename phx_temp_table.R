#' @export

phx_temp_table = function(key, object, df, user, file_path)
{
  # 1. Clone table
  clone_table_query = paste0("CREATE OR REPLACE TABLE TEMP_STAGE CLONE ", 
                             object)
  clone_table = try(phx_write(key, clone_table_query, user), 
                    silent = TRUE)
  
  # 2. Drop unused fields
  all_cols = phx_attributes(key, "TEMP_STAGE", user)
  df_cols = colnames(df)
  
  checking = df_cols %in% all_cols
  
  if (FALSE %in% checking)
  {
    stop("ERROR 1: Column names do not correspond to the given table column names.")
  }
  
  unused_cols = all_cols[all_cols %in% df_cols == FALSE]
  
  if (length(unused_cols) == length(all_cols))
  {
    stop("ERROR 2: Column names do not correspond to the given table column names.")
  }
  
  else
  {
    unused_cols = paste0(unused_cols, collapse = ",")
    
    drop_attributes_query = paste0("ALTER TABLE TEMP_STAGE DROP COLUMN ", unused_cols, ";")
    drop_attributes = try(phx_write(key, drop_attributes_query, 
                                    user), silent = TRUE)
  }
  
  # new_df_cols = all_cols[all_cols %in% df_cols]
  # df = df[,new_df_cols]
  # if (is.null(dim(df)))
  # {
  #   df = as.character(df)
  #   df = data.frame(df)
  #   colnames(df) = new_df_colsz
  # }
  # df = strsplit(df, ";")
   
  # Delete records
  delete_recs = try(phx_write(key,"Delete from TEMP_STAGE",user), silent = TRUE)
  
  # 3. Upload df to temp_stage
  if (class(delete_recs)[1] != "try-error")
  {
    phx_write_csv(df,file_path)
    insert_clone_elements = try(phx_insert(key, "TEMP_STAGE", file_path, user), silent = TRUE)
  }
  
  return (insert_clone_elements)
}
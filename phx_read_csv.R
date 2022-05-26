#' @export

phx_read_csv = function(file_path)
{
  new_df = try(read.csv(file_path, encoding = "UTF-8"
                        , as.is = TRUE), silent = TRUE)

  if (class(new_df) == "try-error")
  {
    new_df = try(read.csv(file_path, encoding = "UTF-16LE"
                          , as.is = TRUE), silent = TRUE)
  }

  if (class(new_df) != "try-error")
  {
    if (nrow(new_df) != 0)
    {
      new_df[is.na(new_df)] = "NULL"
      new_df[new_df == "NA"] = "NULL"
      new_df[new_df == ""] = "NULL"
      new_df[new_df == " "] = "NULL"
    }

    new_df = try(data.frame(lapply(new_df, function(x) {gsub(",", ";", x)})), silent = TRUE)
    new_df = try(data.frame(lapply(new_df, function(x) {gsub("\"", "", x)})), silent = TRUE)
    new_df = try(data.frame(lapply(new_df, function(x) {gsub("[\r\n]", "", x)})), silent = TRUE)

    for (i in 1:ncol(new_df))
    {
      new_df[[i]] = as.character(new_df[[i]])
    }
  }

  return(new_df)
}

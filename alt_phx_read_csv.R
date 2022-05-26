#' @export

alt_phx_read_csv = function(file_path)
{
  print("Trying UTF-8")
  new_df = try(read.csv(file_path, fileEncoding = "UTF-8"
                        , as.is = TRUE), silent = TRUE)

  if (class(new_df) != "try-error" && (length(colnames(new_df)) > 1 && nchar(colnames(new_df)[1]) > 2))
  {
    print("UTF-8 Successful")

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

    return(new_df)
  }

  else
  {
    print("Trying UTF-16LE")
    lines = try(scan(file_path, what = "character", fileEncoding = "UTF-16LE", sep = "\n")
                , silent = TRUE)

    if (length(lines) > 0)
    {
      print("Cleaning UTF-16LE")
      # Remove extra commas
      lines = stringr::str_replace_all(lines, ',","', ',""')

      # Split into list
      lines = strsplit(lines, ',\"')

      # Convert to DF
      df = t(as.data.frame(lines))
      df = as.data.frame(df)

      # Clean DF
      {
        rownames(df) = NULL
        new_df = df

        new_df = try(data.frame(lapply(df, function(x) {
          gsub(",", ";", x)
        })), silent = TRUE)

        # Change single quotes

        new_df = try(data.frame(lapply(new_df, function(x) {
          gsub('\"', '', x)
        })))

        new_df = try(data.frame(lapply(new_df, function(x) {
          gsub('[\r\n]', '', x)
        })))
        for (i in 1:ncol(new_df)) {
          new_df[[i]] = as.character(new_df[[i]])

          # Set column name to element in first row
          colnames(new_df)[i] = as.character(new_df[1,i])
        }

        # Remove 1st row w/ column names
        new_df = new_df[-c(1),]

        if(nrow(new_df) > 0)
        {
          new_df[is.na(new_df)] = "NULL"
          new_df[new_df == "NA"] = "NULL"
          new_df[new_df == ""] = "NULL"
          new_df[new_df == " "] = "NULL"
        }
      }

      return(new_df)
    }

    else
    {
      stop("ERROR: Reading Error")
    }
  }
}

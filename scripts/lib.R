cleanDf <- function(df) {
  if (!is.null(df)) {
    for (col in 1:ncol(df)) {
      if (class(df[,col])[1] == "character") {
        df[,col] <- sapply(df[,col], function(x) {
          x <- gsub("[\r\n]", " ", x)
          x <- gsub("  ", " ", x)
          x <- str_trim(x)
          if (!is.na(x) & x == "") {
            x <- NA
          }
          return(x)
        })
      }
    }
  }
  return(df)
}

fetchLSID <- function(name) {
  res <- wm_records_names(name)[[1]]
  for (j in 1:nrow(res)) {
    if (res$match_type == "exact") {
      return(res$lsid[j])
    }
  }
  return(NA)
}

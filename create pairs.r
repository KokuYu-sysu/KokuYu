#' @param data A data frame containing the variables of interest.
#' @param cols A character vector of column names to create pairs from.
#' @param n The maximum number of variables to include in each pair.

create_pairs <- function(data, cols, n = NULL) {
  for (col in cols) {
    if (!col %in% names(data)) {
      stop(paste("Column", col, "not found in data."))
    }
  }

  if (is.null(n)) {
    n <- length(cols)
  }

  for (j in 2:n) {
    pairs <- combn(cols, j)
    for(i in 1:ncol(pairs)) {
      combination_cols <- pairs[, i]
      new_col_name <- paste(combination_cols, collapse = "_X_")
      data[[new_col_name]] <- rowSums(data[combination_cols])
    }
  }
  return(data)
}

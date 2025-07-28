# When creating diseases pair (like comorbidity), it can be used to.
create_pairs <- function(data, cols) {  # cols means the column number of variable you would combine
  pairs <- combn(cols, 2)
  for(i in 1:ncol(pairs)) {
    col1 <- pairs[1,i]
    col2 <- pairs[2,i]
    new_col_name <- paste0(col1, "_X_", col2)
    data[[new_col_name]] <- data[[col1]] + data[[col2]]  # '+' can sum up the value
  }
  return(data)
}
# Output: 0 means both not, 1 means A or B, and 2 means A and B.
# Theorically, it can add a parameter 'n' in order to create pair including any number of disease.

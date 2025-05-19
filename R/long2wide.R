# Convert data from long format to wide format
long2wide <- function(x) {
  x <- as.data.frame(x)
  obs_col = 1
  group_col = 2
  group_values <- unique(x[[group_col]])
  if (length(group_values) != 2) {
    stop("Data must contain exactly two groups.")
  }
  group1_name <- as.character(group_values[1])
  group2_name <- as.character(group_values[2])

  group1_data <- x[x[[group_col]] == group_values[1], obs_col]
  group2_data <- x[x[[group_col]] == group_values[2], obs_col]

  # Pad with NA to handle unequal lengths
  max_length <- max(length(group1_data), length(group2_data))
  length(group1_data) <- max_length
  length(group2_data) <- max_length

  wdset <- data.frame(group1 = group1_data, group2 = group2_data)
  wdset <- setNames(wdset, c(group1_name, group2_name))
  return (wdset)
}
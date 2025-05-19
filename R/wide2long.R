# Convert data from wide format to long format
wide2long <- function(x, y = NULL, grpnames = NULL) {
  if (is.null(y)) {
    if (ncol(x) != 2) {
      stop("If 'y' is NULL, 'x' must be a data frame or matrix with two columns.")
    }
    original_colnames <- colnames(x)
    y <- x[, 2]
    x <- x[, 1]
    if (is.null(grpnames)) {
      if (!is.null(original_colnames) && length(original_colnames) == 2) {
        grpnames <- original_colnames
      } else {
        grpnames <- c("Group1", "Group2")
        warning("Column names not found or not two columns in 'x', using default group names: Group1 and Group2.")
      }
    }
    nx <- length(x)
    ny <- length(y)
    group <- factor(c(rep(grpnames[1], nx), rep(grpnames[2], ny)), levels = grpnames)
    ldset <- data.frame(obs = c(x, y), group = group)
  } else {
    if (is.null(grpnames)) {
      grpnames <- c("Group1", "Group2")
    }
    nx <- length(x)
    ny <- length(y)
    group <- factor(rep(grpnames, c(nx, ny)), levels = grpnames)
    ldset <- data.frame(obs = c(x, y), group = group)
  }
  return(ldset)
}
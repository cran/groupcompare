# Inverse Normal Transformation (INT)
intnorm <- function(x) {
  if (is.vector(x) && is.numeric(x)) {
    xrank <- rank(x, na.last = "keep")
    xnorm <- qnorm((xrank-0.5) / sum(!is.na(x)))
  } else {
    stop("A numeric vector is required to transform!")
  }
  return(xnorm)
}

# Calculate the difference between the basic stats of two groups in long data format
calcstatdif <- function(x, indices, ...){

  # Skewness function
  skewness <- function(x, type = 3) {
    n <- length(x)
    xm <- mean(x)
    xsd <- sd(x)
    if (is.null(type) || type == 3) {
      xskew <- sum((x - xm)^3) / (n * xsd^3)
    } 
    return(xskew)
  }

  kurtosis <- function(x) {
    n <- length(x)
    xm <- mean(x)
    xsd <- sd(x)
    xkurt <- (sum((x - xm)^4) / n) / (xsd^4) - 3
    return(xkurt)
  }


   if(missing(indices)) indices <- 1:nrow(x)
   dMean <- diff(rev(tapply(x[indices,1], x[indices,2], mean)))
   dMed <- diff(rev(tapply(x[indices,1], x[indices,2], median)))
   dIQR <- diff(rev(tapply(x[indices,1], x[indices,2], IQR)))
   dVar <- diff(rev(tapply(x[indices,1], x[indices,2], var)))
   dSkew <- diff(rev(tapply(x[indices,1], x[indices,2], skewness)))
   dKurt <- diff(rev(tapply(x[indices,1], x[indices,2], kurtosis)))
   delta <- c(dMean, dMed, dIQR, dVar, dSkew, dKurt)
   names(delta) <- c("MEAN", "MED", "IQR", "VAR", "SKEW", "KURT")
   return(delta)
}
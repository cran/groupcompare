# Calculate the difference between Huber means of two groups in long data format
calchubermeandif <- function(x, indices, ...){

  hubermean <- function(x, k = 1.5, tol = 1e-6, max.iter = 100) {
    xm <- median(x)
    for (i in 1:max.iter) {
       w <- pmin(1, k / abs(x - xm))
       xm_new <- sum(w * x) / sum(w)
       if (abs(xm_new - xm) < tol) break
       xm <- xm_new
    }
    return(xm)
  }

   if(missing(indices)) indices <- 1:nrow(x)
   dHuber <- diff(rev(tapply(x[indices,1], x[indices,2], hubermean)))
   delta <- c(dHuber)
   names(delta) <- c("HuberMean")
   return(delta)
}
# Permutation test with long data
permtest <- function(x, y=NULL, statistic, alternative="two.sided", Q=seq(0.1, 0.9, by=0.1), qt=7, R=10000) {
  # Convert data to long format if necessary
  if (!is.null(y)) {
    x <- wide2long(x, y)
  }
  n <- tapply(x[,1], x[,2], length)
  grplev <- unique(x[,2])

  # Calculate the differences for original data
  tetahat <- statistic(x, Q=Q, qt=qt)
  nstats <- length(tetahat)
  tetahatstar <- matrix(NA, nrow=R, ncol=nstats)
  pv <- numeric(nstats)
  pvtwosided <- numeric(nstats)
  pvsk <- numeric(2)
  for (i in 1:R) {
    group_labels <- sample(x[, 2])
    resampx <- x
    resampx[, 2] <- group_labels
    tetahatstar[i,] <- statistic(resampx, Q=Q, qt=qt)
  }

  for (i in 1:nstats) {
    if (alternative == "less") {
      pv[i] <- sum(tetahatstar[,i] < tetahat[i]) / R
    } else if (alternative == "greater") {
      pv[i] <- sum(tetahatstar[,i] > tetahat[i]) / R
    } else if (alternative == "two.sided") {
      pv[i] <- sum(abs(tetahatstar[,i]) >= abs(tetahat[i])) / R
    }else{
      stop("Invalid alternative hyphothesis. Please assign 'less', 'grater' or 'two.sided.")
    }
    pvtwosided[i] <- sum(abs(tetahatstar[,i]) >= abs(tetahat[i])) / R
  }

  pv[pv == 0] <- 2.2e-16
  names(pv) <- names(tetahat)
  colnames(tetahatstar) <- names(tetahat)

  if (identical(statistic, calcstatdif)){
    pvsk[1] <- pvtwosided[5]
    pvsk[2] <- pvtwosided[6]
  }else{
    pvsk <- c(NA, NA)
  }
  
  result <- list(
    t0 = tetahat,
    tstar = tetahatstar,
    pval = pv,
    alternative = alternative,
    R = R,
    pvalsk = pvsk,
    call = match.call()
  )

  return(result)
}

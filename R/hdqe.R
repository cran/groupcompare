# Harrell-Davis Quantile Estimator
hdqe <- function(x, Q = c(0.25, 0.5, 0.75)) {
  n <- length(x)
  xsorted <- sort(x)
  quantiles <- numeric(length(Q))
  for (i in 1:length(Q)) {
      p <- Q[i]
      # Harrell-Davis weight calculation
      weights <- sapply(1:n, function(j) dbeta((j - 0.5) / n, p * (n + 1), (1 - p) * (n + 1)))
      quantiles[i] <- sum(weights * xsorted) / sum(weights)
  }
  return(quantiles)
}

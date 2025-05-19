descstats <- function(x, trim=0.1, k=1.5){

  # Winsorized mean function
  winsorizedmean <- function(x, trim = 0.1) {
    n <- length(x)
    xsorted <- sort(x)
    # Determine the extreme values
    lowertrim <- floor(n * trim) + 1
    uppertrim <- n - floor(n * trim)
  
    # Replace extreme values with inner values
    xsorted[1:lowertrim] <- xsorted[lowertrim]
    xsorted[uppertrim:n] <- xsorted[uppertrim]
  
    winsmean <- mean(xsorted)
    winsvar <- sum((xsorted - winsmean)^2) / (n - 1)
    winsse <- sqrt(winsvar / n)
    statistics <- c(winsmean, winsse, winsvar) 
    return(statistics)
  }

  # Skewness function
  skew <- function(x, type = 3) {
    n <- length(x)
    xm <- mean(x)
    xsd <- sd(x)

    if (is.null(type) || type == 3) {
      # Type 3 skewness
      xskew <- sum((x - xm)^3) / (n * xsd^3)
    } else if (type == 1) {
      # Type 1 skewness
      xskew <- sqrt(n * (n - 1)) / (n - 2) * sum((x - xm)^3) / (n * xsd^3)
    } else if (type == 2) {
      # Type 2 skewness
      xskew <- sqrt(n * (n - 1)) / (n - 2) * sum((x - xm)^3) / (n * xsd^3)
      xskew <- xskew * ((n - 1) / n)^1.5
    }
    return(xskew)
  }

  # Kurtosis function (unbiased estimator)  
  kurtosis <- function(x) {
    n <- length(x)
    xm <- mean(x)
    xsd <- sd(x)
    xkurt <- (sum((x - xm)^4) / n) / (xsd^4) - 3
    return(xkurt)
  }

  hdqe <- function(x, probs) {
    n <- length(x)
    xsorted <- sort(x)
    quantiles <- numeric(length(probs))
    for (i in 1:length(probs)) {
      p <- probs[i]
      # Harrell-Davis weight calculation
      weights <- sapply(1:n, function(j) dbeta((j - 0.5) / n, p * (n + 1), (1 - p) * (n + 1)))
      quantiles[i] <- sum(weights * xsorted) / sum(weights)
    }
    return(quantiles)
  }

  # Huber M-estimator calculation function
  hubermest <- function(x, k = 1.5, tol = 1e-6, niter = 100) {
    xmed <- median(x)  # Use median as the initial value
    for (i in 1:niter) {
      u <- (x - xmed) / k
      w <- pmin(1, k / abs(u))  # Weights
      xmednew <- sum(w * x) / sum(w)  # New estimate 
      if (abs(xmednew - xmed) < tol) break
      xmed <- xmednew
    }
    return(xmed)
  }

  onestepest <- function(x, k = 1.5) {
  # Calculate the median of the data
    xmed <- median(x)
    u <- (x - xmed) / k
    w <- ifelse(abs(u) <= 1, 1, 1 / abs(u))
    # Calculate the one-step estimator
    osmean <- sum(w * x) / sum(w)
    return(osmean)
  }

  # Modified One-step Estimator (MOM) calculation function
  momest <- function(x, k = 1.5) {
    # Calculate the median of the data
    xmed <- median(x)
    # Calculate the weights
    u <- (x - xmed) / mad(x)
    w <- ifelse(abs(u) <= k, 1, k / abs(u))
    mommean <- sum(w * x) / sum(w)
    return(mommean)
  }

   if(is.numeric(x) && is.vector(x)){
     n <- length(x)
     xmin <- min(x)
     xmax <- max(x)
     xmean <- mean(x)
     xtrmean <- mean(x, trim = trim)
     xmed <- median(x)
     xsd <- sd(x)
     xse <- sd(x) / sqrt(length(x))
     xmad <- mad(x, center = median(x))
     xrange <- diff(range(x))
     xiqr <- IQR(x)
     xskew <- skew(x)
     xkurt <- kurtosis(x)
     xwinsmean <- winsorizedmean(x, trim=trim)[1]
     xhubmean <- hubermest(x, niter=500)
     descriptives <- c(n, xmin, xmax, xmean, xse, xtrmean, xmed, xmad, 
       xskew, xkurt, xwinsmean, xhubmean, xrange, xiqr, xsd)  
     names(descriptives) <- c("n", "min", "max", "mean", "se", "trmean", "med", "mad",
       "skew", "kurt", "winsmean", "hubermean", "range", "iqr", "sd")  
     descriptives <- as.list(descriptives)
   }else{
     stop("A numeric vector must be input.")
   }
   return(descriptives)
}

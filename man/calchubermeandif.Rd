\name{calchubermeandif}
\alias{calchubermeandif}
\title{The difference between Huber's M-estimator of location}
\description{
  Computes the difference between Huber's M-estimator of location of two groups in long data format.
}
\usage{
calchubermeandif(x, indices, ...)
}
\arguments{
  \item{x}{A data frame or matrix containing the input data. The first column should be the variable of interest, and the second column should be the grouping variable.}
  \item{indices}{Optional; specific rows to be considered. If not provided, all rows are used.}
  \item{...}{Additional arguments passed to the internal hubermean function.}
}
\details{
  This function demonstrates the structure of a user-defined statistic function to use with bootstrap and permutation test. The function calculates the difference between Huber's M-estimator of location of two groups using the iterative weighted mean method. Huber's M-estimator of location is robust to outliers and is computed using an iterative re-weighting procedure. The internal function follows:

  \itemize{
    \item Initialize with the median of the data.
    \item Compute weights based on deviations from the current mean.
    \item Update the mean iteratively until convergence is reached.
  }
}
\value{
  A numeric value representing the difference between Huber's M-estimator of location of the two groups.
}
\seealso{
  \code{\link[stats]{median}}
}
\author{
Zeynel Cebeci, A. Firat Ozdemir, Engin Yildiztepe
}
\examples{
df <- data.frame(value = rnorm(100), group = rep(1:2, each = 50))
bivarplot(df)

# Bootstrap confidence intervals for the difference of 
# Huber's M-estimator of location of two groups
# Increase R for real-world applications
bshubermean <- bootstrap(df, statistic=calchubermeandif, alpha=0.05,
   alternative="less", R=200)
bshubermean
}
\concept{Huber mean}
\concept{robust statistics}

\keyword{robust statistics}
\keyword{mean}
\keyword{outlier}
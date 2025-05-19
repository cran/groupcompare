\name{calcquantdif}
\alias{calcquantdif}
\title{Quantile Differences}
\description{
  Calculates the differences between specified quantiles for grouped data.
}
\usage{
calcquantdif(x, indices, Q=seq(0.1, 0.9, 0.1), qt=7)
}
\arguments{
  \item{x}{A data frame or matrix containing the input data. The first column should be the variable of interest, and the second column should be the grouping variable.}
  \item{indices}{Optional; specific rows to be considered. If not provided, all rows are used.}
  \item{Q}{A numeric vector specifying the quantiles to be computed. The default is \code{seq(0.1, 0.9, 0.1)}.}
  \item{qt}{An integer specifying the quantile type from 0 to 9. The default is type 7, as discussed by Hyndman and Fan (1996)<doi:10.2307/2684934>.}
}
\details{
  This function calculates the differences between specified quantiles for groups defined by the second column of the input data. It uses the specified quantile type to compute the quantiles. Types of quantiles are:

\itemize{
  \item \bold{0}: Harrell-Davis estimator (not available in stats::quantile function).
  \item \bold{1}: Inverse of the empirical distribution function.
  \item \bold{2}: Similar to Type 1 but with averaging at discontinuities.
  \item \bold{3}: Empirical distribution with sampling.
  \item \bold{4}: Linear interpolation of the empirical distribution function.
  \item \bold{5}: Linear interpolation of the expectations for the order statistics.
  \item \bold{6}: Linear interpolation of the modes for the order statistics.
  \item \bold{7}: The default in the stats::quantile function.
  \item \bold{8}: Median-unbiased estimator.
  \item \bold{9}: Normal-unbiased estimator.
}
For the details on types, see the \code{\link[stats]{quantile}} and \code{\link{hdqe}} function.
}
\value{
  A numeric vector containing the differences between the specified quantiles for each group.
}
\seealso{
  \code{\link[stats]{quantile}}, \code{\link{hdqe}}, \code{\link{calcstatdif}}
}
\author{
Zeynel Cebeci, A. Firat Ozdemir, Engin Yildiztepe
}

\references{
  Hyndman, R. J. and Fan, Y. (1996) Sample quantiles in statistical packages, \emph{American Statistician, 50}, 361-365. <doi:10.2307/2684934>.
}

\examples{
df <- data.frame(value = rnorm(100), group = rep(1:2, each = 50))

# Difference between the medians
mediandif <- calcquantdif(df, Q=0.5)
mediandif

# Differences between the quartiles
quantdifs <- calcquantdif(df, Q=c(0.25, 0.5, 0.75))
quantdifs

# Differences between the percentiles from P10 to P90 using the method 5
quants <- seq(0.1, 0.9, 0.1)
quantdifs <- calcquantdif(df, Q=quants, qt=5)
quantdifs

# Differences between the percentiles from P10 to P90 using the method Harrell-Davis
quants <- seq(0.1, 0.9, 0.1)
quantdifs <- calcquantdif(df, Q=quants, qt=0)
quantdifs
}

\concept{quantiles}
\concept{quantile difference}

\keyword{quantile}
\keyword{quartile}
\keyword{decile}
\keyword{percentile}
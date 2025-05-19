\name{calcquantile}
\alias{calcquantile}
\title{Sample Quantiles}
\description{
  Calculates the quantiles (percentiles) for a given vector of data at specified fractions.
}
\usage{
calcquantile(x, indices, Q = seq(0.1, 0.9, 0.1), qt = 7)
}
\arguments{
  \item{x}{Numeric vector containing the values to calculate quantiles.}
  \item{indices}{Optional; vector containing the indices for which the calculation will be performed.}
  \item{Q}{Probabilities for quantile levels. The default is \code{seq(0.1, 0.9, 0.1)}}
  \item{qt}{Type of quantile calculation. Integer between \code{0} and \code{9}. Default: \code{7}}
}
\details{
  This function calculates the quantiles at specified fractions of the given data set. If \code{qt} is 0, the \code{\link{hdqe}} function is used.
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
For the details on types, see the \code{\link[stats]{quantile}} and \code{\link{hdqe}} functions.
}
\value{
  Returns a numeric vector containing the calculated quantiles.
}
\author{
Zeynel Cebeci, A. Firat Ozdemir, Engin Yildiztepe
}
\references{
  Hyndman, R. J. and Fan, Y. (1996) Sample quantiles in statistical packages, \emph{American Statistician} \bold{50}, 361--365. <doi:10.2307/2684934>.
}

\seealso{
  \code{\link[stats]{quantile}}, \code{\link{hdqe}}
}
\examples{
x <- rnorm(100)
calcquantile(x)
calcquantile(x, qt=9)
calcquantile(x, qt = 0)
}

\name{hdqe}
\alias{hdqe}
\title{Harrell-Davis Quantile Estimator}
\description{
  Computes the Harrell-Davis quantile estimator for given quantile levels.
}
\usage{
hdqe(x, Q = c(0.25, 0.5, 0.75))
}
\arguments{
  \item{x}{Numeric vector of data values.}
  \item{Q}{A numeric vector of quantile levels to estimate, between 0 and 1. Defaults to \code{c(0.25, 0.5, 0.75)} for the 25th, 50th, and 75th percentiles (Q1, Q2, Q3).}
  }
\value{
  A numeric vector containing estimated quantiles.
}
\details{
  The function computes the Harrell-Davis quantile estimator, which estimates data quantiles by calculating 
  a weighted average of order statistics. The weights are based on the beta distribution.
}
\author{
Zeynel Cebeci, A. Firat Ozdemir, Engin Yildiztepe
}
\examples{
set.seed(1)
x <- sample(1:10, 50, replace=TRUE)
quantile(x, probs=c(0.25, 0.5, 0.75), type=1) # quantiles with Type 1 in stats::quantile
quantile(x, probs=c(0.25, 0.5, 0.75), type=7) # quantiles with Type 7 (default) in stats::quantile
hdqe(x, Q=c(0.25, 0.5, 0.75)) # quantiles with Harrell-Davis Estimator
}

\concept{quantiles}

\keyword{harrell-davis estimator}
\keyword{quantile}
\keyword{quartile}
\keyword{decile}
\keyword{percentile}
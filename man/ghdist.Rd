\name{ghdist}
\alias{ghdist}
\title{Random Sample from G-H Distribution}
\description{
  Generates a random sample from the G-and-H (GH) distribution with specified parameters.
}
\usage{
ghdist(n = 30, A = 0, B = 1, g = 0, h = 0)
}
\arguments{
  \item{n}{An integer specifying the sample size. The default is 30.}
  \item{A}{A numeric value specifying the location parameter. The default is 0.}
  \item{B}{A numeric value specifying the scale parameter. The default is 1. Must be positive.}
  \item{g}{A numeric value specifying the skewness parameter. The default is 0.}
  \item{h}{A numeric value specifying the kurtosis parameter. The default is 0. Must be zero or positive.}
}
\details{
  The gh distribution is a flexible distribution defined by four parameters: A (location), B (scale), g (skewness), and h (kurtosis). The parameter B must be positive, and h must be zero or positive. This function generates random samples from the gh distribution using these parameters.

  The GH distribution was introduced by John W. Tukey in 1977 as a way to model data with varying degrees of skewness and kurtosis. The distribution is defined by transforming standard normal random variables using the g and h parameters to control skewness and kurtosis, respectively.
}
\value{
  A numeric vector of length \code{n} containing the generated random samples.
}
\seealso{
  \code{\link{rnorm}}
}
\author{
Zeynel Cebeci, A. Firat Ozdemir, Engin Yildiztepe
}
\examples{
 set.seed(50) 
 A <- ghdist(100, 50, 2, g=0, h=0)
 B <- ghdist(100, 50, 3, g=0.5, h=0.5)
 ds <- data.frame(A=A, B=B)
 head(ds)
 bivarplot(ds)
}

\references{
  Tukey, J. W. (1977). \emph{Exploratory Data Analysis}. Addison-Wesley.
}

\concept{random sampling}
\concept{probability distribution}
\concept{G&H distribution}

\keyword{distribution}
\keyword{sampling}
\keyword{statistics}

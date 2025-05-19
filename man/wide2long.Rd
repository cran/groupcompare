\name{wide2long}
\alias{wide2long}
\title{Convert Wide-Format Data to Long-Format Data}
\description{
  Converts wide-format data to long-format data.
}
\usage{
wide2long(x, y = NULL, grpnames=NULL)
}
\arguments{
  \item{x}{A data frame or matrix containing two columns for the observations in groups.}
  \item{y}{Optional; a vector containing data to combine with \code{x}. If \code{NULL}, the second column of \code{x} will be used as \code{y}.}
  \item{grpnames}{Optional character string vector for the names of groups, e.g. c("A","B"). }
}
\details{
  This function converts wide-format data to long format by combining two columns of data into a single column and creating a grouping variable to distinguish the original data sources.
}
\value{
  A data frame with two columns: \code{Obs} containing the combined values of \code{x} and \code{y}, and \code{group} indicating the original data source.
}
\seealso{
  \code{\link{long2wide}}
}
\author{
Zeynel Cebeci, A. Firat Ozdemir, Engin Yildiztepe
}
\examples{
 # Normal distributed groups of data with equal means and different variances
 set.seed(2)
 grp1 <- rnorm(20, 50, 5)
 grp2 <- rnorm(20, 50, 9)
 ds1 <- data.frame(A=grp1, B=grp2)
 head(ds1)
 bivarplot(ds1)

 # Convert to long data
 ds2 <- wide2long(ds1)
 head(ds2)
}

\concept{data shape}
\concept{data reshape}

\keyword{data structure}
\keyword{long data}
\keyword{wide data}

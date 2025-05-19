\name{ci2df}
\alias{ci2df}
\title{Convert List to Data Frame}
\description{
  Converts a list of confidence intervals into a data frame.
}
\usage{
ci2df(x)
}
\arguments{
  \item{x}{A list where each element is a list of data frames or matrices containing confidence interval data as the result of the function \code{\link{bootstrap}}. }
}
\details{
  This function takes a list of confidence intervals and converts it into a data frame. Each row represents a method, and each column represents a statistic. Confidence intervals are formatted as strings in the form \code{[lower, upper]}.
}
\value{
  A data frame with rows representing methods and columns representing statistics, containing the confidence intervals.
}
\seealso{
  \code{\link{bootstrap}}
}
\author{
Zeynel Cebeci, A. Firat Ozdemir, Engin Yildiztepe
}
\examples{
ciresults <- list(
  method1 = data.frame(lower = c(-0.1, 0.2), upper = c(0.3, 0.4), row.names = c("stat1", "stat2")),
  method2 = data.frame(lower = c(0.2, 0.3), upper = c(0.4, 0.5), row.names = c("stat1", "stat2"))
)
ciresults

cidf <- ci2df(ciresults)
cidf
}

\concept{confidence interval}
\concept{data frame}
\concept{list conversion}

\keyword{confidence interval}
\keyword{data frame}
\keyword{list}

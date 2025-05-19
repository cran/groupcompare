\name{long2wide}
\alias{long2wide}
\title{Convert Long-Format Data to Wide-Format Data}
\description{
  Converts long-format data to wide-format data by splitting based on groups.
}
\usage{
long2wide(x)
}
\arguments{
  \item{x}{A data frame or matrix with two columns. The first column must contain observations, and the second column must contain the group levels as factor.}
}
\details{
    This function converts long-format data to wide format by splitting it based on unique groups in the second column. The resulting data frame has columns for each group, where each column contains the values of the first column for the corresponding group.
}
\value{
   A data frame with columns corresponding to unique groups and rows containing the values for each group.
}
\seealso{
  \code{\link{wide2long}}
}
\author{
Zeynel Cebeci, A. Firat Ozdemir, Engin Yildiztepe
}
\examples{
# Normal distributed groups of data with different means and different variances
set.seed(21)
obs1 <- rnorm(20, 50, 5)
obs2 <- rnorm(20, 55, 3)
obs <- c(obs1, obs2)
grp <- c(rep(as.factor("A"), 20), rep(as.factor("B"), 20))
ds1 <- data.frame(obs = obs, group = grp)
head(ds1)

# Reshape data to long format
ds2 <- long2wide(ds1)
head(ds2)
bivarplot(ds2)
}

\concept{data shape}
\concept{data reshape}

\keyword{data structure}
\keyword{long data}
\keyword{wide data}

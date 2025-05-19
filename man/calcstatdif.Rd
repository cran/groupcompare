\name{calcstatdif}
\alias{calcstatdif}
\title{Differences of Basic Descriptive Statistics}
\description{
  Calculates the differences in multiple statistics (mean, median, IQR, variance) for grouped data.
}
\usage{
calcstatdif(x, indices, ...)
}
\arguments{
  \item{x}{A data frame or matrix containing the input data. The first column should be the variable of interest, and the second column should be the grouping variable.}
  \item{indices}{Optional; specific rows to be considered. If not provided, all rows are used.}
  \item{...}{Optional arguments;}
}
\details{
  This function calculates the differences in multiple statistics (mean, median, interquartile range (IQR), and variance) for groups defined by the second column of the input data. The function is used in permutation test and 
bootstrapping as a statistical function.
}
\value{
  A named numeric vector containing the differences in the specified statistics for each group. The names of the vector elements are \code{"MEAN"}, \code{"MED"}, \code{"IQR"}, \code{"VAR"}, \code{"SKEW"}, and \code{"KURT"} for mean, median, interquartile range, variance, skewness, and kurtosis, respectively.
}
\seealso{
  \code{\link{calcquantdif}}
}
\author{
Zeynel Cebeci, A. Firat Ozdemir, Engin Yildiztepe
}

\examples{
# Generate example data 
set.seed(1199)  
grp1 <- rnorm(20, 45, 5)
grp2 <- c(rnorm(10, 45, 10), rnorm(10, 52, 20))
df <- data.frame(cbind(grp1=grp1, grp2=grp2))
head(df)
bivarplot(df)

# Reshape the data into long format
df <- wide2long(df)
head(df)

# Differences between the basic stats
calcstatdif(df)
}

\concept{descriptive statistics}
\concept{basic statistics}
\concept{grouped data analysis}

\keyword{statistics}
\keyword{difference}
\keyword{group}

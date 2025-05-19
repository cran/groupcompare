\name{permtest}
\alias{permtest}
\title{Permutation Test on Long-Format Data}
\description{
  Performs a permutation test on long-format data to evaluate differences between two groups using a specified test statistic.
}
\usage{
permtest(x, y = NULL, statistic, alternative = "two.sided",
   Q = seq(0.1, 0.9, by = 0.1), qt = 7, R = 10000)
}

\arguments{
  \item{x}{A data frame or matrix containing the data in long format.}
  \item{y}{An optional second data frame or matrix. If provided, the data will be combined with \code{x} and converted to long format.}
  \item{statistic}{The function name to compute the statistic of interest.}
  \item{alternative}{Type of alternative hypothesis: \code{"two.sided"}, \code{"less"}, or \code{"greater"}. The default is \code{"two.sided"}, corresponding to a two-tailed test.}
  \item{Q}{A vector of quantile probabilities specifying the quantiles to be compared. The default is \code{seq(0.1, 0.9, by = 0.1)}, corresponding to the 10th through 90th percentiles (P10 to P90).}
  \item{qt}{An integer between 0 and 9 specifying the quantile calculation method. The default is 7.}
  \item{R}{An integer indicating the number of permutations. The default is 10000.}
}

\details{
  The function allows researchers to perform robust statistical testing by utilizing permutations. This approach does not rely on distributional assumptions and is particularly useful when the sample size is small or the data distribution is unknown. The test generates an empirical distribution of the test statistic by repeatedly permuting group labels and recalculating the statistic, providing p-values based on these permutations.
}
\value{
  A list containing the following components:
  \item{t0}{The estimated statistics of the differences.}
  \item{t}{A matrix of the permuted values of the statistics.}
  \item{pval}{A numeric vector of p-values for each statistic.}
  \item{alternative}{The specified alternative hypothesis.}
  \item{R}{The number of permutations.}
  \item{pvalsk}{A numeric vector of p-values for skewness and kurtosis with two-sided alternative. These are used to check the similarity of distributions to decide whether to use a bootstrap and a permutation test for inference.}
  \item{call}{The matched call.}
}
\seealso{
  \code{\link{bootstrap}}
}
\author{
Zeynel Cebeci, A. Firat Ozdemir, Engin Yildiztepe
}
\examples{
\donttest{
# Group1 normal, Group 2 right skewed with equal means and different variances
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

# Permutation test for the differences between the basic stats of two groups
result <- permtest(df, statistic = calcstatdif, alternative = "two.sided", R = 500)
result$pval

# Permutation with custom statistics
# A custom function to compute the differences between the group means
meancomp <- function(x, ...){
   meandif <- diff(rev(tapply(x[,1], x[,2], mean)))
   return(meandif)
}

# Permutation test with meancomp function
result <- permtest(x = df, statistic = meancomp, alternative="less", R=500)
result$pval
}
}

\concept{statistical testing}
\concept{resampling statistics}
\concept{permutation test}

\keyword{permutation}
\keyword{statistical test}
\keyword{non-parametric}
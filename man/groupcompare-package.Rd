\name{groupcompare-package}
\alias{groupcompare-package}
\docType{package}
\title{Comparing Two Groups with Various Descriptive Statistics}

\description{
The ` groupcompare` package performs various statistical tests to compare two groups. It calculates descriptive statistics and quantile statistics and then conducts some normality tests and variance homogeneity tests. Based on these assumptions checks, it provides results from t-test or Wilcoxon rank sum test, permutation tests, and bootstrap confidence intervals.
}

\details{
The main function `groupcompare` of the package is designed to compare two independent or paired groups using various statistical tests. It calculates descriptive statistics and quantile statistics. Then it performs Shapiro-Wilk normality tests, variance homogeneity test (Levene's test), t-test, Wilcoxon signed-rank sum test (or Mann-Whitney U test), permutation tests, and bootstrap confidence intervals.
 \describe{
   \item{\code{\link{groupcompare}}}{The main function which compares descriptive statistics of two groups using a variety of statistical tests.}
   \item{\code{\link{bivarplot}}}{Generates various plots to visualize and compare the distribution and characteristics of two variables.}
   \item{\code{\link{bootstrap}}}{Calculates bootstrap confidence intervals for the descriptive statistics or any statistic implemented in a custom function.}
   \item{\code{\link{calchubermeandif}}}{Computes the difference between Huber???s M-estimator of location of two groups in long data format.}
   \item{\code{\link{calcquantdif}}}{Calculates the differences between specified quantiles for grouped data.}
   \item{\code{\link{calcquantile}}}{Calculates the quantiles (percentiles) for a given vector of data at specified fractions.}
   \item{\code{\link{calcstatdif}}}{Calculates the differences in multiple statistics (mean, median, IQR, variance) for grouped data.}
   \item{\code{\link{ci2df}}}{Converts a list of confidence intervals into a data frame.}
   \item{\code{\link{descstats}}}{Calculates the common and robust descriptive statistics.}
   \item{\code{\link{ghdist}}}{Generates a random sample from the g-and-h (gh) distribution with specified parameters.}
   \item{\code{\link{groupdata}}}{A data set contains seven data frames with two variables from various distributions.}
   \item{\code{\link{hdqe}}}{Computes the Harrell-Davis quantile estimator for given quantile levels.}
   \item{\code{\link{intnorm}}}{Performs an inverse normal transformation on non-normally distributed data.}
   \item{\code{\link{levene.test}}}{Performs the Levene test to check the homogeneity of variances across groups.}
   \item{\code{\link{long2wide}}}{Converts long-format data to wide-format data by splitting based on groups.}
   \item{\code{\link{permtest}}}{Performs a permutation test on long-format data to evaluate differences between two groups using a specified test statistic.}
   \item{\code{\link{quail}}}{A data frame containing daily weight gains (in grams) of two quail breeds during a fattening period.}
   \item{\code{\link{wide2long}}}{Converts wide-format data to long-format data.}
}
}

\author{Zeynel Cebeci, A. Firat Ozdemir, Engin Yildiztepe}

\seealso{
 \code{\link{bootstrap}},
 \code{\link{permtest}},
 \code{\link{ghdist}},
 \code{\link{bivarplot}}
}

\examples{
\donttest{
# Sample dataset in long format
set.seed(123)
group1 <- rnorm(30, mean=50, sd=2)
group2 <- rnorm(30, mean=51, sd=3)
df <- data.frame(value=c(group1, group2), group=rep(c("A", "B"), each=30))

# Compare the groups using various descriptive statistics
result <-  groupcompare(df, cl=0.95, alternative="two.sided",
  q=c(0.25, 0.5, 0.75), qt=0, R=500, out=FALSE, verbose=TRUE)
result

# Compare the groups using Huber's M-estimator of location with bootstrap
bshubermean <- bootstrap(df, statistic=calchubermeandif, 
  alternative="two.sided", alpha=0.05, R=500)
bshubermean

permhubermean <- permtest(df, statistic=calchubermeandif, 
  alternative="two.sided", R=500)
permhubermean$pval
}
}

\concept{data analysis}
\concept{statistical tests}
\concept{permutation test}
\concept{quantiles comparison}
\concept{non-parametric tests}

\keyword{permutation}
\keyword{bootstrap}
\keyword{parametric tests}
\keyword{bivariate statistics}

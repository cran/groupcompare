\name{bootstrap}
\alias{bootstrap}
\title{Bootstrap for Various Statistics}
\description{
  Conducts a bootstrap procedure to calculate confidence intervals for various statistics. 
}
\usage{
bootstrap(x, statistic, alternative="two.sided", alpha = 0.05, 
  Q = c(0.25, 0.5, 0.75), qt = 7, R = 3000)
}
\arguments{
  \item{x}{A data frame or matrix containing the data.}
  \item{statistic}{Name of the function to compute the statistic of interest.}
  \item{alternative}{Type of hypothesis test. The default is "two.sided".}
  \item{alpha}{A numeric value specifying the significance level for confidence intervals. The default is 0.05.}
  \item{Q}{A numeric vector or a number specifying the probabilities used in quantile calculations. The default is c(0.25, 0.5, 0.75) for P25, P50, P75 (aka Q1, Q2 and Q3).}
  \item{qt}{A numeric value specifying the type of quantile calculation. The default is 7.}
  \item{R}{An integer specifying the number of bootstrap replicates. The default is 3000.}
}
\details{
  This function performs a bootstrap procedure to calculate confidence intervals for various statistics. It is mainly used to evaluate the differences between various statistics for two groups based on a specified function. The function calculates confidence intervals using different methods, including normal, basic, percentile, and bias-corrected and accelerated (BCa) intervals. It allows users to pass custom statistics 
(via \code{statistic}) that include parameters like quantiles (via \code{Q}) and types of quantiles (\code{qt}), making it versatile
 for non-standard use cases. In this way, the function extends the capabilities of \code{boot::boot.ci} (R's suggested package) by supporting 
more advanced statistical needs and customizable interval calculations. This function also incorporates the argument \code{alternative} can be set to \code{"greater"} or \code{"less"} for one-tailed confidence intervals, whereas \code{boot::boot.ci} primarily focuses on two-tailed intervals.Therefore, it is particularly useful for specialized applications. The data can be provided in long format, and the test uses a specified number of bootstrap replicates to calculate the empirical distribution of the test statistic under the null hypothesis. 
}
\value{
  A list containing the data frames for the following components for each statistic:
  \item{normal}{Lower and upper limits of the confidence interval computed with the normal method.}
  \item{basic}{Lower and upper limits of the confidence interval computed with the basic method.}
  \item{percent}{Lower and upper limits of the confidence interval computed with the percent method.}
  \item{bca}{Lower and upper limits of the confidence interval computed with the BCa method.}
}
\seealso{
  \code{\link{permtest}}
}
\author{
Zeynel Cebeci, A. Firat Ozdemir, Engin Yildiztepe
}
\examples{
# Load the quail dataset 
data(quail)

# Bootstrap for the difference of various basic statistics 
# Increase R for real-world applications

bootres <- bootstrap(quail, statistic = calcstatdif, R=200)
bootres

# Arrange the results as a data frame
ci2df(bootres)

# Bootstrap for the differences of quantiles 
bootresq <- bootstrap(quail, statistic = calcquantdif, R=200)
bootresq

# Arrange the results as a data frame
ci2df(bootresq)
}

\concept{statistical testing}
\concept{resampling statistics}
\concept{bootstrapping}

\keyword{bootstrap}
\keyword{statistical test}
\keyword{resampling}
\keyword{non-parametric}

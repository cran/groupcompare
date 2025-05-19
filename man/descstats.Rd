\name{descstats}
\alias{descstats}
\title{Descriptive Statistics}
\description{
  This function calculates various descriptive statistics for a given numeric vector. These statistics include measures of central tendency, dispersion, skewness, kurtosis, and some robust estimators.
}
\usage{
descstats(x, trim = 0.1, k = 1.5)
}
\arguments{
  \item{x}{A numeric vector.}
  \item{trim}{The fraction (0 to 0.5) of observations to be trimmed from each end of the vector is used to calculate the trimmed mean and winsorized mean. The default is 0.1.}
  \item{k}{The robustness parameter for the Huber M-estimator. The default is 1.5.}
}
\details{
In order to determine an appropriate \emph{k} value for th Huber M-estimator some experiments might be needed. In the literature, commonly used \emph{k} values typically range from 1.5 to 2. Users can start by choosing any value within this range. However, to determine an appropriate \emph{k} within a given range, it is also selected by performing Huber estimations for each \emph{k} value within this range, as shown in the example below. In the output, the estimated Huber M-estimator values can be checked on a plot. Select \emph{k} values where a smooth trend or plateau is reached. If the Huber M-estimator values stabilize after a certain \emph{k} value, that \emph{k} value may be appropriate. Finally, if there are outliers and you want to reduce their impact, you can use smaller \emph{k} values.
}
\value{
  A list containing the computed descriptive statistics, including:
    \item{n}{The number of observations}
    \item{min}{The minimum value}
    \item{max}{The maximum value}
    \item{mean}{The mean}
    \item{se}{The standard error of the mean}
    \item{sd}{The standard deviation}
    \item{trmean}{The trimmed mean}
    \item{med}{The median}
    \item{mad}{The median absolute deviation (MAD), a robust statistic for measuring variability in data.}
    \item{skew}{The skewness}
    \item{kurt}{The excess kurtosis measures how peaked or flat a distribution is compared to a normal distribution. Subtracting 3 centers the measure relative to the kurtosis of a normal distribution, which is always 3.}
    \item{winsmean}{The Winsorized mean}
    \item{hubermean}{The Huber's M-estimator of location}
    \item{range}{The range}
    \item{iqr}{The interquartile range}
}
\author{
  Zeynel Cebeci, A. Firat Ozdemir, Engin Yildiztepe
}
\examples{
set.seed(123)
x <- rnorm(100, mean=50, sd=5)
descriptives <- descstats(x)
as.data.frame(descriptives)

descriptives$mean
descriptives$se

# Determining the appropriate k in a given set of different k values. 
# This parameter is used to calculate the Huber M-estimator of the location
# Array of k values for testing
k <- seq(0, 5, by = 0.1)

result <- sapply(k, function(k) (descstats(x, k = k)$hubermean))
names(result) <- paste0("k=", k)
result

plot(k, result, type = "b", col = "blue", pch = 19, ylab = "Huber's mean")

descstats(x, k=2, trim=0.05)$hubermean
}



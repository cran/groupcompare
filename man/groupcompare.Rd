\name{groupcompare}
\alias{groupcompare}
\title{Comparing Two Groups with Various Descriptive Statistics}
\description{
  Performs a variety of statistical tests and generates plots to compare two groups.
}
\usage{
 groupcompare (ds, paired=FALSE, cl=0.95, alternative="two.sided",
   qtest=TRUE, q=seq(0.1, 0.9, by=0.1), qt=7, lognorm=TRUE, 
   R=3000, plots=TRUE, out=FALSE, verbose=TRUE)
}
\arguments{
 \item{ds}{A data frame or matrix with the first column representing the first group and the second column the second group.}
 \item{paired}{A logical flag indicating whether the group data is paired (i.e., related). Set to TRUE for paired groups.}
 \item{cl}{Confidence level for the interval. The default is 0.95. The significance level (Alpha, Type I error) equals 1-cl.}
 \item{alternative}{Type of alternative hypothesis: $\code{"two.sided"}$, $\code{"less"}$, or $\code{"greater"}$. The default is $\code{"two.sided"}$, corresponding to a two-tailed test.}
 \item{qtest}{Logical; if $\code{TRUE}$, performs a quantile test. The default is $\code{TRUE}$.}
 \item{q}{A vector of quantile probabilities specifying the quantiles to be compared. The default is $\code{seq(0.1, 0.9, by = 0.1)}$, corresponding to the 10th through 90th percentiles (P10 to P90).}
 \item{qt}{An integer between 0 and 9 specifying the quantile calculation method. The default is 7.}
 \item{lognorm}{Logical; if $\code{TRUE}$, performs a logarithmic transformation on data. The default is $\code{TRUE}$.}
 \item{R}{An integer indicating the number of permutations or bootstrap samples. The default is 3000.}
 \item{plots}{Logical; if $\code{TRUE}$, generates bivariate plots for the data. The default is $\code{TRUE}$.}
 \item{out}{Logical; if $\code{TRUE}$, writes the result to a file named $\code{' groupcompare.txt'}$.}
 \item{verbose}{Logical; if $\code{TRUE}$ displays the details for each test. The default is $\code{TRUE}$.}
}
\details{
  This function calculates descriptive statistics and performs a t-test or Wilcoxon test on two groups of data, depending on the variance homogeneity test and normality test. It also generates various plots, including density plots, ECDF plots, boxplots, violin plots, QQ plots, symmetry plots, and empirical shift plots, to visualize the data.
}
\value{
  A list that contains the results of the statistical tests and the bivariate plots, if \code{plots} is \code{TRUE}.
  \item{descriptives}{A data frame containing summary statistics, such as mean, standard deviation, and sample size, for each group.}
  \item{quantiles}{A data frame with calculated quantile values for specified probabilities for each group.}
  \item{test}{The outcome of the appropriate statistical test (e.g., t-test or Wilcoxon test), including test statistic and p-value.}
  \item{quantest}{A data frame containing quantile comparison results if \code{qtest} is set to \code{TRUE}.}
  \item{inference}{A string summarizing the conclusion of the statistical test performed, indicating whether the null hypothesis was rejected or not.}
}
\seealso{
  \code{\link{bootstrap}}, \code{\link{permtest}}, \code{\link{bivarplot}}
}
\author{
Zeynel Cebeci, A. Firat Ozdemir, Engin Yildiztepe
}
\examples{
\donttest{
   # Test with default values
   df <- data.frame(grp1 = rnorm(50), grp2 = rnorm(50))
   df <- wide2long(df)
   result <-  groupcompare(df)
   print(result)

   # Test with daily weight gain of two quail breeds
   data(quail)
   result <-  groupcompare(quail, alternative="two.sided", cl=0.95,
     R=200, plots=TRUE, out=FALSE, verbose=TRUE)
   print(result)

   # Test with milk trace mineral data
   filepath <- system.file("extdata", "milkcomp.csv", package = "groupcompare")
   milktrace <- read.table(filepath, header=TRUE, sep=",")
   head(milktrace) 
   milkzinc <- as.data.frame(cbind(milktrace$Zn, milktrace$grp))
   colnames(milkzinc) <- c("zn","grp")
   head(milkzinc)
   tail(milkzinc)

   result <-  groupcompare(milkzinc, cl=0.99, alternative="greater",
      R=200, plots=TRUE, out=FALSE, verbose=TRUE)
   print(result)
 }
}

\concept{data analysis}
\concept{statistical testing}
\concept{visualization}

\keyword{analysis}
\keyword{statistical testing}
\keyword{visualization}

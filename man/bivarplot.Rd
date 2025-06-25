\name{bivarplot}
\alias{bivarplot}
\title{Plots for Two Variables}
\description{
  Generates various plots to visualize and compare the distribution and characteristics of two variables.
}
\usage{
bivarplot(ds)
}
\arguments{
  \item{ds}{A data frame or matrix containing the input data. The first column should be the variable of interest, and the second column should be the grouping variable, if data is in long format.}
}
\details{
  This function generates a series of plots to compare two variables, including density plots, ECDF plots, boxplots, violin plots, QQ plots, symmetry plots, and empirical shift plots. If data is in long format, the function uses the second column of \code{ds} for the group label.
}
\value{
  Generates a series of plots to the current graphical device.
}
\author{
Zeynel Cebeci, A. Firat Ozdemir, Engin Yildiztepe
}
\examples{
# Create data for two independent groups, each with 50 observations
set.seed(1)
df1 <- data.frame(value = rnorm(100), group = rep(1:2, each = 50))
head(df1)

# Plots for visualization
bivarplot(df1)

# Convert to long data to wide data
df2 <- long2wide(df1)
head(df2)

# Plots for visualization
bivarplot(df2)
}

\concept{data visualization}
\concept{plotting}
\concept{comparative analysis}

\keyword{visualization}
\keyword{plot}
\keyword{comparison}
\keyword{bivariate}

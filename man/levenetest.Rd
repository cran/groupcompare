\name{levene.test}
\alias{levene.test}
\title{Levene Test for Homogeneity of Variances}
\description{
  This function performs the Levene test to check the homogeneity of variances across groups.
}
\usage{
levene.test(ds)
}
\arguments{
  \item{ds}{
    A data frame with two columns: the first column contains numeric values, and the second column contains group labels.
  }
}
\details{
  The function calculates the absolute deviations from the group means, performs an ANOVA on these deviations, and returns the p-value of the Levene test.
}
\value{
  A single numeric value named \code{Levene.p}, representing the p-value of the Levene test.
}
\author{
  Zeynel Cebeci, A. Firat Ozdemir, Engin Yildiztepe
}
\examples{
df <- data.frame(values = c(1.1, 2.3, 2.5, 3.7, 1.2, 2.1, 3.4, 3.9),
  groups = c("A", "A", "B", "B", "A", "A", "B", "B"))
levene.test(df)
}

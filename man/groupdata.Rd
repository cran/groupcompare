\name{groupdata}
\alias{groupdata}
\docType{data}
\title{Synthetic Data for Two Independent Groups}
\description{
  This data set contains seven data frames with two variables from various distributions.
}
\usage{
 data(groupdata)
}
\format{
  A list containing 7 data frames:
  \describe{
    \item{df1}{Normally distributed, equal means, and equal variances. Columns: A, B.}
    \item{df2}{Normally distributed, unequal means, and unequal variances. Columns: A, B.}
    \item{df3}{Normally distributed, equal means, and unequal variances. Columns: A, B.}
    \item{df4}{A normal, B right-skewed, equal means, and unequal variances. Columns: A, B.}
    \item{df5}{A normal, B right-skewed, equal means, and unequal variances. Columns: A, B.}
    \item{df6}{A normal, B right-skewed, equal means, and unequal variances. Columns: A, B.}
    \item{df7}{A normal, B right-skewed, equal means, and unequal variances. Columns: A, B.}
  }
}
\author{
 Zeynel Cebeci, A. Firat Ozdemir, Engin Yildiztepe
}
\examples{
  data(groupdata)
  df1 <- groupdata$df1
  head(df1)
  bivarplot(df1)

  df4 <- groupdata$df4
  head(df4)
  bivarplot(df4)
}


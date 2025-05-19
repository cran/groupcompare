\name{quail}
\alias{quail}
\docType{data}
\title{Daily Weight Gains of Quail Breeds}
\description{
  A data frame containing daily weight gains (in grams) of two quail breeds during a fattening period.
}
\usage{data(quail)}
\format{
  A data frame with 16 observations on the following 2 variables:
  \describe{
    \item{dwg}{A numeric vector containing daily weight gains (g).}
    \item{genotype}{A factor with levels \code{A} and \code{B} indicating breeds or genotypes.}
  }
}
\details{
  The dataset is used to compare the daily weight gain performances of different breeds of quails.
}
\examples{
data(quail)
head(quail)
}

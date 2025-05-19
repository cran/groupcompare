\name{intnorm}
\alias{intnorm}
\title{Inverse Normal Transformation Function}
\usage{
intnorm(x)
}
\arguments{
  \item{x}{A numeric vector of data to be transformed.}
}
\description{
This function performs an inverse normal transformation on non-normally distributed data. Using the provided rank information, it can also revert the transformed data back to its original scale.
}
\value{
A numeric vector of the transformed values.
}

\author{
Zeynel Cebeci, A. Firat Ozdemir, Engin Yildiztepe
}

\examples{
set.seed(1)
xnonnormal <- c(rexp(25, rate = 1), rexp(25, rate = 0.2))
xnormal <- intnorm(xnonnormal)

# Plot density and perform the Shapiro-Wilk test for non-normal and normalized data
opar <- par(mfrow = c(1, 2))

# Non-normal data plot
density_xnonnormal <- density(xnonnormal)
plot(density_xnonnormal, main = "Density Plot of Non-Normal Data", 
     xlab = "Value", ylab = "Density", col = "blue", lwd=2)
polygon(density_xnonnormal, col = rgb(1, 0, 0, 0.3))
shapirotest1 <- shapiro.test(xnonnormal)
mtext(paste("p-value:", round(shapirotest1$p.value, 4)), 
      side = 3, line = 0.5, at = mean(xnonnormal), col = "black")
shapirotest1

# Normalized data plot
density_xnormal <- density(xnormal)
plot(density_xnormal, main = "Density Plot of Inverse Normalized Data", 
     xlab = "Value", ylab = "Density", col = "blue", lwd=2)
polygon(density_xnormal, col =  rgb(1, 0, 0, 0.3)) 
shapirotest2 <- shapiro.test(xnormal)
mtext(paste("p-value:", round(shapirotest2$p.value, 4)), 
      side = 3, line = 0.5, at = mean(xnormal), col = "black")
shapirotest2

par(opar)
}

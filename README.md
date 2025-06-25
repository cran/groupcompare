# groupcompare: Comparing Two Groups with Various Descriptive Statistics

## Introduction

The `groupcompare` package performs various statistical tests to compare two independent or paired groups. It calculates descriptive statistics, quantile statistics, and conducts normality and variance homogeneity tests. Based on these assumption checks, it provides results from t-test, Wilcoxon rank sum test, permutation tests, and bootstrap confidence intervals.

## Features

- Descriptive and quantile statistics for two independent or paired groups
- Normality test using Shapiro-Wilk test
- Variance homogeneity test using Levene's test
- Parametric (t-test) and non-parametric (Wilcoxon rank sum test) comparisons
- Permutation tests for all types of statistics and alternative hypotheses.
- Bootstrap confidence intervals for all types of statistics and alternative hypotheses.


## Installation

You can install the latest version of `groupcompare` from CRAN using:

```r
install.packages("groupcompare", repos="cloud.r-project.org", dep=TRUE)
```

Or install the development version from GitHub using:

```r
remotes::install_github("zcebeci/groupcompare")
```

## Usage

Some examples:

```r
# Load package
library(groupcompare)

# Sample dataset in long format
set.seed(123)
group1 <- rnorm(30, mean=50, sd=2)
group2 <- rnorm(30, mean=51, sd=3)
df <- data.frame(value=c(group1, group2), group=rep(c("A", "B"), each=30))

# Plot the groups
bivarplot(df)

# Compare the groups using various descriptive statistics
result <- groupcompare(df, cl=0.95, alternative="two.sided",
  q=c(0.25, 0.5, 0.75), qt=0, R=5000, out=FALSE, verbose=TRUE)
print(result)

# Compare groups using Huber means with bootstrap
bshubermean <- bootstrap(df, statistic=calchubermeandif, 
  alternative="two.sided", alpha=0.05, R=5000)
print(bshubermean)

permhubermean <- permtest(df, statistic=calchubermeandif, 
  alternative="two.sided", R=10000)
print(permhubermean$pval)

```

# Functions Overview
-groupcompare(): Main function to compare the descriptive statistics of two groups.
- descstats(): Computes common and robust descriptive statistics.
- calcquantile(): Computes quantile statistics including Harrel-Davis estimator.
- bootstrap(): Calculates bootstrap confidence intervals for descriptive statistics and user-defined statistics.
- permtest(): Performs permutation tests for descriptive statistics and user-defined statistics.
- ghdist(): Generates synthetic data simulating G&H distributions.
- bivarplot(): Generates various plots for visualizing two-variable distributions.

# Authors
- Zeynel Cebeci
- A. Firat Ozdemir
- Engin Yildiztepe

# License

This package is released under the GPL (>= 2) License.

## Citation

If you use `groupcompare` in your research or work, please cite it as follows:

Cebeci, Z. Ozdemir, A. F., & Yildiztepe, E. (2025). groupcompare: Comparing Two Groups with Various Descriptive Statistics (Version 1.0.1). Available from https://CRAN.R-project.org/package=groupcompare







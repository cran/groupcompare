## ----eval=FALSE, message=FALSE, warning=FALSE---------------------------------
# install.packages("groupcompare", dep=TRUE)

## ----eval=TRUE, message=FALSE, warning=FALSE----------------------------------
library("groupcompare")

## ----eval=TRUE, message=FALSE, warning=FALSE----------------------------------
 set.seed(12) # For reproducibility purpose
 grp1 <- ghdist(50, 50, 2, g=0, h=0)
 grp2 <- ghdist(50, 45, 4, g=0.8, h=0)
 ds1 <- data.frame(grp1=grp1, grp2=grp2)
 head(ds1)

 # Data in long format
 ds2 <- wide2long(ds1)
 head(ds2)

## ----fig.width=8, fig.height=7------------------------------------------------
bivarplot(ds2)

## ----eval=TRUE, message=FALSE, warning=FALSE----------------------------------
results <- permtest(ds2, statistic=calcquantdif, alternative="two.sided", R=500)

## ----eval=TRUE, message=FALSE, warning=FALSE----------------------------------
str(results)
results$t0
head(results$tstar)
results$pval


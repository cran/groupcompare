## ----eval=FALSE, message=FALSE, warning=FALSE---------------------------------
# install.packages("groupcompare", dep=TRUE)

## ----eval=TRUE, message=FALSE, warning=FALSE----------------------------------
library("groupcompare")

## ----eval=TRUE, message=FALSE, warning=FALSE----------------------------------
 set.seed(30) # For reproducibility purpose
 grp1 <- ghdist(25, 50, 2, g=0, h=0)
 grp2 <- ghdist(25, 50, 2, g=0, h=0)
 ds1 <- data.frame(grp1=grp1, grp2=grp2)
 head(ds1)

 # Data in long format
 ds2 <- wide2long(ds1)
 head(ds2)

## ----fig.width=8, fig.height=7------------------------------------------------
bivarplot(ds2)

## ----eval=TRUE, message=FALSE, warning=FALSE----------------------------------
results <- groupcompare(ds2, alternative="two.sided", cl=0.95, qtest=FALSE, 
R=1000, plots=FALSE, out=FALSE, verbose=TRUE)

## ----eval=TRUE, message=FALSE, warning=FALSE----------------------------------
results$descriptives
results$meanstest
results$inference


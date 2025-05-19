levene.test <- function(ds) {
  devs <- abs(ds[,1] - ave(ds[,1], ds[,2], FUN = mean))
  devsdf <- data.frame(devs = devs, groups = ds[,2])
  aovres <- aov(devs ~ groups, data = devsdf)
  pval <- summary(aovres)[[1]][["Pr(>F)"]][1]
  names(pval) <- "p.value"
  return(pval)
}

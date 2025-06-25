# Plots for two variables
bivarplot <- function(ds){
   oldpar <- par(no.readonly = TRUE)
   on.exit(par(oldpar)) 
   par(mfrow=c(3, 3))
   ngrup <- length(unique(ds[,2]))
   if (ngrup == 2){
      ds <- long2wide(ds)
   }
   
   if (!is.null(colnames(ds))) {
      gn <- colnames(ds)
   } else {
      gn <- c("Grp1", "Grp2")
   }
   
   x <- ds[,1]
   y <- ds[,2]
   
   minx <- min(density(x)$x, density(y)$x)
   maxx <- max(density(x)$x, density(y)$x)
   miny <- min(density(x)$y, density(y)$y)
   maxy <- max(density(x)$y, density(y)$y)
   minecdfx <- min(min(x), min(y))
   maxecdfx <- max(max(x), max(y))

   # Density plots
   plot(density(x), lwd=1, col=2, 
     xlim=c(minx,maxx), ylim=c(miny,maxy),
     xlab="", ylab="density", main="Density Distribution Plots")
   lines(density(y), lwd=1, col=4)
   legend("topleft", col=c(2,4), legend=gn, lty=c(1,1),  bty="n")
   plot(stats::ecdf(x), lwd=1, col=2, xlab="",
     xlim=c(minecdfx, maxecdfx), main="ECDF Plots")
   lines(stats::ecdf(y), lwd=1, col=4)
   legend("topleft", col=c(2,4), legend=gn, 
     lty=c(1,1), bty="n")

   # Boxplots 
   boxplot(x,y, cex=1, col=c(2,4), names=gn, pch=21,
     medcol="white", medlwd=1, main="Boxplots")

   # Violin plots 
   vioplot::vioplot(x, y, names=gn, col="gray90", border=4, 
   rectCol=c(2,4), colMed=7, pchMed=19, horizontal=FALSE)
   title("Violin Plots")

   # QQ plots 
   qqnorm(x, pch=21, bg="gray90", xlab="Theoretical quantiles", 
     ylab="Sample quantiles", main=paste("QQ Plot (", gn[1],")"))
   qqline(x, col=2)
   qqnorm(y, pch=21,bg="gray90", xlab="Theoretical quantiles", 
     ylab="Sample quantiles", main=paste("QQ Plot (", gn[2],")"))
   qqline(y, col=4)

   # Symmetry plots
   sortedx <- sort(x)
   sortedy <- sort(y)
   n <- length(x)
   svalx <- sortedx - rev(sortedx)
   svaly <- sortedy - rev(sortedy)
   plot(svalx, type = "l", col = "red", lwd = 2,
     main=paste("Symmetry Plot (", gn[1],")"), xlab = "Index", ylab = "Symmetry Value")
   abline(a = 0, b = 0, col = "gray", lty = 2)
   plot(svaly, type = "l", col = "blue", lwd = 2,
     main=paste("Symmetry Plot (", gn[2],")"), xlab = "Index", ylab = "Symmetry Value")
   abline(a = 0, b = 0, col = "gray", lty = 2)

   # Empirical shift plot
   probs <- seq(0.1, 0.99, 0.1)
   qx <- quantile(x, probs)
   qy <- quantile(y, probs)
   shift <- qx - qy
   plot(probs, shift, type = "b", pch = 19, col = "blue", xlab = "Quantiles", ylab = "Shift",
      main = "Empirical Shift Plot")
   abline(h = 0, lty = 3, lwd=2, col="red")      
}

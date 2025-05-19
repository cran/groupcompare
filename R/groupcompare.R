# Statistical tests for comparing two independent groups
groupcompare <- function(ds, paired=FALSE, cl=0.95, alternative="two.sided",
   qtest=TRUE, q=seq(0.1, 0.9, by=0.1), qt=7, lognorm=TRUE,
   R=3000, plots=TRUE, out=FALSE, verbose=TRUE){
   
   # Check whether the data is long-formatted
   groups <- unique(ds[,2])
   if(length(groups) != 2)
     stop("Data must be long-formmatted and has two groups only!")
   grpnames <- groups
   grp1 <- ds[ds[,2] == groups[1], 1]
   grp2 <- ds[ds[,2] == groups[2], 1]

   nameds <- deparse(substitute(ds))
   outfile <- paste0("grouptest_", nameds, ".out")
   
   if(out) sink(outfile)
   if(plots) bivarplot(ds)

   dstat1 <- unlist(descstats(grp1))
   dstat2 <- unlist(descstats(grp2))
   dstat <- rbind(dstat1, dstat2)
   rownames(dstat) <- grpnames
   
   qstat1 <- calcquantile(grp1, Q=q, qt=qt)
   qstat2 <- calcquantile(grp2, Q=q, qt=qt)
   qstat <- rbind(qstat1, qstat2)
   rownames(qstat) <- grpnames

   if(verbose){
      message(paste("\nCOMPARISON ANALYSIS for: ", nameds, "\n\n"))  
   }

   alpha <- 1 - cl	# Significance level

   if(alternative=="two.sided"){ 
       sigmsg <- "different from zero" 
   } else if(alternative=="greater"){ 
       sigmsg <- "greater than zero" 
   } else if(alternative=="less"){ 
       sigmsg <- "less than zero" 
   } else {
       stop("Alternative should be one of 'two.sided', 'greater' or 'less'.") 
   }   
   
   if(verbose){
      message("\nDESCRIPTIVE STATISTICS:\n")
      print (dstat)
      message("\nQUANTILE STATISTICS:\n")
      print (qstat)
   }
   
   # Normality check with Shapiro-Wilk's test
   shapirotest1 <- shapiro.test(grp1)
   shapirotest2 <- shapiro.test(grp2)
   
   if(verbose){
      message("\nNORMALITY TESTS:\n")
      print(shapirotest1)
      print(shapirotest2)
   }
   
   # Flags to control normality
   normflag1 <- shapirotest1$p.value > alpha
   normflag2 <- shapirotest2$p.value > alpha

   if(verbose){
     message(ifelse (normflag1, "Group 1 is normal distributed.\n", "Group 1 is not normal distributed.\n"))
     message(ifelse (normflag2, "Group 2 is normal distributed.\n", "Group 2 is not normal distributed.\n"))
   }
   
   npt <- !(normflag1 & normflag2)
   
   # Log-transformation of group data
   if(lognorm){
     if(!normflag1 || !normflag2){
       message("Data is being normalized with logarithmic transformation...\n")
       if(any(grp1 <= 0) || any(grp2 <= 0)){
         tgrp1 <- log(grp1 + abs(min(grp1)) + 1)
         tgrp2 <- log(grp2 + abs(min(grp2)) + 1)
       }else{
         tgrp1 <- log(grp1)
         tgrp2 <- log(grp2)
       }
       # Normality check after log normalization
       tshapirotest1 <- shapiro.test(tgrp1)
       tshapirotest2 <- shapiro.test(tgrp2)
       if(verbose){
         message("\nNORMALITY TESTS AFTER LOG TRANSFORMATION:\n")
         print(tshapirotest1)
         print(tshapirotest2)
       }
       # Flags to control normality
       tnormflag1 <- tshapirotest1$p.value > alpha
       tnormflag2 <- tshapirotest2$p.value > alpha
       message(ifelse (tnormflag1, "Log-transformed Group 1 is normal distributed.\n", "Log-trasformed Group 1 is not normal distributed.\n"))
       message(ifelse (tnormflag2, "Log-transformed Group 2 is normal distributed.\n", "Log-transformed Group 2 is not normal distributed.\n"))
       if(tnormflag1 && tnormflag2){
         grp1 <- tgrp1
         grp2 <- tgrp2
         npt <- FALSE
       }
     }
   }
        
   # Levene's Test to check variance homogenity 
   levtest <- levene.test(ds)
   if(verbose){
      message("\nLEVENE'S TEST FOR VARIANCE HOMOGENITY:\n")
      print(levtest)
   }
   if(levtest < alpha){
     vareq <- FALSE
     message("According to Levene's test, the group variances are not homogenous. The assumption of homogeneity of variances is not satisfied.\n")
   } else {
      vareq <- TRUE
      message("According to the Levene's test, the group variances are homogenous. The assumption of homogeneity of variances is satisfied.\n")
   }   
    
   # T-TEST
   ttest <- t.test(grp1, grp2, alternative = alternative, var.equal = vareq, paired = paired)
   if(verbose){ 
     print(ttest)
   }
   ttestres <- c(ttest$statistic, t.p=ttest$p.value)
   ttestreport <- paste0("According to the t-test, the difference of the group means is ",
   ifelse(ttest$p.value < alpha, " significantly ", " not significantly "), sigmsg, ", ", 
         "t(", dstat[1,1],",", dstat[2,1], ")=", round(ttest$statistic, 3), ", p=", round(ttest$p.value,4), ".")

   # NON-PARAMETRIC TEST
   if(verbose){
     if(paired)
       message("\nWILCOXON SIGNED RANK TEST:\n")
     else
       message("\nWILCOXON RANK SUM TEST (MANN-WHITNEY U TEST)\n")
   }
   nptest <- wilcox.test(grp1, grp2, alternative=alternative, paired=paired)
   if(verbose){
     print(nptest)
   }
   nptestres <- c(nptest$statistic, W.p = nptest$p.value)
   nptestreport <- paste0("According to the", ifelse(paired, " Wilcoxon Signed Rank Test,", 
     " Wilcoxon Rank Sum Test,"), " the difference of the groups is ",
     ifelse(nptest$p.value < alpha, " significantly ", " not significantly "), sigmsg, ", ", 
     "W(", dstat[1,1],",", dstat[2,1], ")=", round(nptest$statistic, 3), ",",  
     sprintf("p = %.4e",nptest$p.value),".")
   
   # PERMUTATION TEST FOR DESCRIPTIVE STATISTICS
   permres <- permtest(ds, statistic=calcstatdif, alternative=alternative, R=R)
   if(verbose){
     message("\nPERMUTATION TEST P-VALUES FOR DESCRIPTIVE STATISTICS:\n")
     print(permres$pval)
   }
   pertestres <- c(p.per.mean=permres$pval[1], p.per.median=permres$pval[2])
   pertestreport <- paste0("According to the permutation test, the difference of the group means is",
     ifelse(pertestres[1] <= alpha, " significantly ", " not significantly "), sigmsg, ",",
     sprintf("p = %.4e",pertestres[1]),".")
   
   # Check the similarity of distributions with skewness and kurtosis 
   # If only used with calcstatdif
   skewsig <- permres$pvalsk[1] <= alpha
   kurtsig <- permres$pvalsk[2] <= alpha
   
   # BOOTSTRAP FOR DESCRIPTIVE STATISTICS
   bootres <- bootstrap(ds, statistic=calcstatdif, alternative=alternative, alpha=alpha, Q=q, qt=qt, R=R)
   boottestres <- c(mean.bca=bootres[[1]][3,], med.bca=bootres[[2]][3,])
   bootsig <- (bootres[[1]][3,1] <= 0) && (bootres[[1]][3,2] >= 0)
   bootreport <- paste0("According to the bootstrap, the difference of the group means is",
     ifelse(!bootsig, " significantly ", " not significantly "), sigmsg, ",",  
     cl*100,"% BCa.CI=[", round(bootres[[1]][3,1], 3), ",", round(bootres[[1]][3,2], 3), "].")   
   
   if(verbose){
     message("\nBOOTSTRAP CIs (BCa) FOR THE DIFFERENCES OF CENTRAL TENDENCY MEASURES:\n")
     print(boottestres)
   }
          
   # BOOTSTRAP CONFIDENCE INTERVALS FOR QUANTILE'S DIFFERENCES
   if (qtest){     
      qtestres1 <- bootstrap(ds, statistic=calcquantdif, alternative=alternative, alpha=alpha, Q=q, qt=qt, R=R)
      if(verbose){
         message("\nBOOTSTRAP CONFIDENCE INTERVALS FOR QUANTILE'S DIFFERENCES:\n")
         print(ci2df(qtestres1))
      }
     
      dif <- apply(t(qstat), 1, function(x) x[1] - x[2])
      qdifs <- cbind(t(qstat), dif)
      colnames(qdifs) <- c("Grp1","Grp2", "Dif")
      
      qtestres2 <- permtest(ds, statistic=calcquantdif, alternative=alternative, Q=q, qt=qt, R=R)
      if(verbose){
        message("\n P-VALUES FOR QUANTILE'S DIFFERENCES OBTAINED WITH PERMUTATION TEST:\n")
        print(qtestres2$pval)
      }
      
      qtboot2 <- ci2df(qtestres1) 
      names(qtboot2) <- c("CI Normal", "CI Basic", "CI Percentile", "CI BCa")
      qtdf <- as.data.frame(cbind(qdifs, qtboot2, perm.p=qtestres2$pval))
    }
   
   # Determine the statistical analysis report
   if(npt && (skewsig || kurtsig)){
      report <- c(bootreport, pertestreport)
      test <- boottestres
   }else if(npt){
      report <- nptestreport
      test <- nptestres 
   }else{
      report <- ttestreport
      test <- ttestres 
   }
   
   result <- list(descriptives = dstat, quantiles = qstat, meanstest=test)
   if (qtest) {
      result$quantest <- qtdf
   }
   result$inference <- report

   if(out) sink()
   
   return(result)
}
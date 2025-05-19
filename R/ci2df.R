# Convert bootstrap CI results to a dataframe
ci2df <- function(x) {
   ci_df <- do.call(rbind, lapply(seq_along(x), function(i) {
     cimethod <- x[[i]]
     data.frame(
       Method = rownames(cimethod ),
       Statistic = ifelse(is.null(names(x)[i]), paste("CI_", i, sep=""), names(x)[i]),
       CI = apply(cimethod, 1, function(row) paste0("[", round(row["lower"],3), ",", round(row["upper"],3), "]"))
     )
   }))
   
   cidf <- reshape(ci_df, idvar = "Method", timevar = "Statistic", direction = "wide")
   rownames(cidf) <- cidf$Method
   cidf$Method <- NULL
   
   # Name of the columns
   colnames(cidf) <- gsub("CI.", "", colnames(cidf))
   
   cidf <- as.data.frame(t(cidf))
   return(cidf)
}

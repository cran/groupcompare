# Calculate the quantiles
calcquantile <- function(x,  indices, Q=seq(0.1, 0.9, 0.1), qt=7){
   if(missing(indices)) indices <- 1:length(x)
   Q <- sort(Q)
   n <- length(Q)
   quants <- numeric(n)
   for (i in 1:n){
      if(qt > 0 && qt < 10){
         quants[i] <- quantile(x[indices], probs=Q[i], type=qt)
      } else if(qt == 0){
        quants[i] <- hdqe(x[indices], Q=Q[i])
      } else{
         stop("Invalid quantile calculation type. Input an integer between 0 and 9.")
      }
   }
   names(quants) <- paste0("P", Q*100)
   return(quants)
}

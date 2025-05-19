# Calculate the difference between the quantiles of two groups using long data format
calcquantdif <- function(x,  indices, Q = seq(0.1, 0.9, 0.1), qt = 7){
   if(missing(indices)) indices <- 1:nrow(x)
   Q <- sort(Q)
   n <- length(Q)
   difQ <- numeric(n)
   for (i in 1:n){
     if(qt>0 && qt<10){
        difQ[i] <- diff(rev(tapply(x[indices,1], x[indices,2], function(y) quantile(y, probs=Q[i], type=qt))))
     } else if(qt==0){
         difQ[i] <- diff(rev(tapply(x[indices,1], x[indices,2], function(y) hdqe(y, Q=Q[i]))))
    } else{
        stop("Invalid quantile calculation type. Input an integer between 1 and 9.")
     }
   }
   names(difQ) <- paste0("P", Q*100)
   return(difQ)
}

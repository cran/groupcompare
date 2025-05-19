# G-and-H distribution
ghdist <- function(n=30, A=0, B=1, g=0, h=0){
   if(sign(B)!=1 | sign(h)==-1) 
     stop("B should be positive; h should zero or positive!")
   Z <- rnorm(n)
   if (g!=0){
     ghval <- (exp(g*Z)-1)*exp(h*Z^2/2)/g
   }else{
     ghval <- Z*exp(h*Z^2/2)
   }
   y <- A + B * ghval
   return(y)
 }
 
   

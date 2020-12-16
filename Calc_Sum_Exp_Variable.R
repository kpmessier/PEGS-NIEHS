psEXP <- function(d,C0,decay) { 
# d : distance matrix (m x n) m = # of monitoring sites, n = # of sources
#  C0 : initial source concentrations
# decay: vector of buffer values (presumably in the same units as d)
  X.ps <- matrix(NA,nrow = nrow(d),ncol = length(decay))
 
  decay.func <- function(x) sum(C0 * exp(-3 * x / decay[i]))
  
    for (i in 1:length(decay)){
      X.ps[,i] <- d %>% apply(1,decay.func)
    }
  
return(X.ps)

}

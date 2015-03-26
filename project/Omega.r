#function to calculate the performance metric Omega
#inputs:
#   -a realization of returns
#   -L (can be a vector), which specifies the point at which we define losses, e.g. 0, risk free rate or some benchmark
Omega <- function(returns,L) {
   
    library(pracma)
    sudoCDF = ecdf(returns)
    a = min(returns)
    b = max(returns)

    Omega = matrix(nrow=length(L),ncol=2)
    colnames(Omega) <- c('L','Omega')
    Omega[,1] = L
    j = 1
    for (i in L) {  
        Omega[j,2] = ( (b - i) - quad(sudoCDF,i,b) ) / quad(sudoCDF,a,i) 
        j = j+1
    }

    return(Omega)
} 

    

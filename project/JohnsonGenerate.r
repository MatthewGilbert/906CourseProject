# R wrapper for Generating Johnson Distributions with desired first 4 moments
#Inputs:
#   First 4 moments and the desired number of simulations
JohnsonGenerate <- function(mu,sigma,skew,kurt, N) {

    library(JohnsonDistribution)

    #calculate the johnson family parameters
    params <- FitJohnsonDistribution(mu, sigma, skew, kurt)
    ITYPE <- params[1]
    GAMMA <- params[2]
    DELTA <- params[3]
    XLAM <- params[4]
    XI <- params[5]
    IFAULT <- params[6]

    if (ITYPE == 5)
        stop("Invalid parameters lead to unsupported distribution boundary case (5)")
    if (IFAULT != 0)
        stop("WARNING: Error exit, JNSN. IFAULT !=  0")
    
    output <- yJohnsonDistribution(rnorm(N), ITYPE, GAMMA, DELTA, XLAM, XI)
}
    

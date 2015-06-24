show_mu_sigma <- function( mu, sigma ) {
  x <- c( mu-sigma, mu, mu+sigma)
  y <- c(0,0,0)
  points(x,y,pch=17,cex=2.5,col=c("magenta","darkred","magenta") )
  #abline( v=mu,   col='darkblue')
  #abline( v=mu-sigma,col='blue')
  #abline( v=mu+sigma,col='blue')
}
# =======================================================================================
# Bernouilli Trials

plotbinom <- function( N, p, show_normal=FALSE ) {
    # number of successes x in N trials
    mu    <- N*p    
    sigma <- sqrt(N*p*(1-p))
    
    x <- seq( from = 0, to = N )
    y <- dbinom( x, N, p )
    
    plot(x,y,type="h",col="red",xlab='x',ylab='P(X=x)',main=sprintf( "Binomial (Number of Successes) N = %d, p = %6.2f, mu=%6.1f, sigma=%6.1f", N, p, mu,sigma ))
    show_mu_sigma(mu,sigma)

    if (show_normal) {
       yb <- dnorm( x, mu, sigma )
       points( x,yb,col="blue" )
    }
}
# ---------------------------------------------------------------------------------------
plotgeom <- function( p, showFirstN = 50 ) {
    # number of trials until first success
    mu    <- 1/p    
    sigma <- sqrt(1-p)/p
  
    x  <- seq( from=0,to=showFirstN-1 )
    y  <- dgeom(x, p )
    x  <- seq( from=1,to=showFirstN )
  
    plot(x,y,type="h",col="red",xlab='x',ylab='P(X=x)',main=sprintf( "Geometric (first success), p =%8.4f, mu=%6.1f, sigma=%6.1f", p, mu, sigma ))
    show_mu_sigma(mu,sigma)
}
# ---------------------------------------------------------------------------------------
plotnbinom <- function( n, np, p ) {
    # number of trials until n'th success
    mu    <- n/p    
    sigma <- sqrt(n*(1-p))/p
  
    x  <- seq( from = 0, to = np-1 ) # number of failures
    y  <- dnbinom( x, n, p )
    x  <- seq( from = n, to = np+n-1 )

    plot(x,y,type="h",col="red",xlab='x',ylab='P(X=x)',main=sprintf( "NegBinomial n=%d, N=%d, p=%5.2f, mu=%6.1f, sigma=%6.1f", n, np, p, mu, sigma ))
    show_mu_sigma(mu,sigma)
}
# =======================================================================================
# Selection without replacement

plothyper <- function( n1, n2, n, show_binom = FALSE ) {
    # n1 possible successes, n2 possible failures, n number of selected items
    N     <- n1+n2

    p     <- n1/N
    mu    <- n*p
    sigma <- sqrt( n*p*(1-p) * (N-n)/(N-1) )

    x <- seq( from=0, to=n)
    y <- dhyper( x, n1, n2, n )

    plot(x,y,type="h",col="red",xlab='x',ylab='P(X=x)',main=sprintf( "HyperGeom, pick %d from (%d,%d), mu=%6.1f, sigma=%6.1f", n, n1, n2, mu, sigma ))
    show_mu_sigma(mu,sigma)

    if (show_binom) {
       yb <- dbinom( x, n, p )
       points( x,yb,col="blue" )
    }
}
# ---------------------------------------------------------------------------------------
plotpoisson <- function( lambda, extent=20, show_normal=FALSE) {
    # Constant Rate of occurrence of events
    mu    <- lambda
    sigma <- sqrt(lambda)

    x <- seq( from=0,to=extent )
    y <- dpois(x, lambda)

    plot(x,y,type="h",col="red",xlab='x',ylab='P(X=x)',main=sprintf( "Poisson, lambda T = %8.4f, mu=%6.1f, sigma=%6.1f", lambda , mu, sigma))
    show_mu_sigma(mu,sigma)
    if (show_normal) {
       yb <- dnorm( x, mu, sigma )
       points( x,yb,col="blue" )
    }
}

# --------------------------------------------
lambdaT   <- 4.
# --------------------------------------------
def_poisson <- function() {
     x     <- 0:12
     y     <- dpois(x,lambdaT)
     mu    <- lambdaT
     sigma <- sqrt(lambdaT)

     xlim  <- c(0,12)
     draw  <- function() plotpoisson( lambdaT, x, y, mu, sigma, xlim )

     func  <- function(n) rpois(n, lambdaT)
     list(sampling_function=func,x=x,y=y,xlim=xlim,draw=draw,mu=mu,sigma=sigma)
}
# --------------------------------------------
def_normal <- function() {
     x     <- seq(from=-3,to=3,by=.1)
     y     <- dnorm(x)
     mu    <- 0.
     sigma <- 1.

     xlim  <- c(-3,3)
     draw  <- function() plotnormal( x, y, mu, sigma, xlim )

     list(sampling_function=rnorm,x=x,y=y,xlim=xlim,draw=draw,mu=mu,sigma=sigma)
}
# --------------------------------------------
def_gaussianmixture <- function() {
     dfunc <- function(x) (dnorm(x,mean=-3,sd=.5) + .8*dnorm(x,mean=3,sd=.2))/1.8
     rfunc <- function(n) (rnorm(n,mean=-3,sd=.5) + .8*rnorm(n,mean=3,sd=.2))/1.8

     # mu = E( X1+X2) = E(X1)+E(X2) = (-3 + .8*3)/1.8 = -1/3
     # sigma^2 = E( (X1+X2)^2 ) - mu^2  = E(X1^2 + X2^2 + 2 X1 X2) - mu2
     mu    <- -1/3
     sigma <- 0.292
     #  estimate:    0.1561739
     #sqrt( (.5^2 292

     x     <- seq(from=-4,to=4,by=.05)
     y     <- dfunc(x)

     cat( 'mean', mean(y), ' sd', sd(y))

     xlim  <- c(-4,4)
     draw  <- function() plotgaussianmixture( x, y, mu, sigma, xlim )

     list(sampling_function=rfunc,x=x,y=y,xlim=xlim,draw=draw,mu=mu,sigma=sigma)
}
# --------------------------------------------
def_cauchy <- function() {
     x     <- seq(from=-3,to=3,by=.1)
     y     <- dcauchy(x)
     mu    <- NA
     sigma <- NA

     xlim  <- c(-3,3)
     draw  <- function() plotcauchy( x, y, xlim )

     list(sampling_function=rcauchy,x=x,y=y,xlim=xlim,draw=draw,mu=mu,sigma=sigma)
}
# --------------------------------------------
def_binomial <- function() {
    N     <- 20
    p     <- .5

    mu    <- N*p    
    sigma <- sqrt(N*p*(1-p))
    
    x <- seq( from = 0, to = N )
    y <- dbinom( x, N, p )

    xlim  <- c(6,17)
    draw  <- function() plotbinom( N, p, x, y, mu, sigma, xlim )

    func1 <- function(n) rbinom(n, N, p)
    list(sampling_function=func1,x=x,y=y,xlim=xlim,draw=draw,mu=mu,sigma=sigma)
}
# --------------------------------------------
def_exponential <- function() {
    lambda <- 1/1.5
    x     <- seq( from=0,to=6, by=.1 )
    y     <- dexp(x, lambda)

    mu    <- 1/lambda
    sigma <- 1/lambda

    xlim  <- c(0,6)
    draw  <- function() plotexponential( x, y, mu, sigma, xlim )

    func1 <- function(n) rexp(n, lambda)
    list(sampling_function=func1,x=x,y=y,xlim=xlim,draw=draw,mu=mu,sigma=sigma)
}
# --------------------------------------------
def_uniform <- function() {
    x     <- seq(from=-1,to=1,by=.01)
    y     <- dunif(x,-1,1)

    mu    <- 0
    sigma <- 1/3

    xlim  <- c(-1.5,1.5)
    draw  <- function() plotuniform( x, y, mu, sigma, xlim )

    func1 <- function(n) runif(n,-1,1)
    list(sampling_function=func1,x=x,y=y,xlim=xlim,draw=draw,mu=mu,sigma=sigma)
}
# --------------------------------------------
adj_density <- function( data, height=0, mu=0 ) {
    # compute the density and adjust the values

    d <- density(data)
    if ( mu == 0 ) {
       d$X <- d$x
    } else {
       d$X <- d$x + mu
    }
    if ( height > 0 ) {
       d$Y <- height * d$y / max(d$y)
    } else {
       d$Y <- d$y
    }
    d$xlim <- c( round(min(d$X)-.3,1), round(max(d$X+.3),1))
    d$ylim <- c(                    0, round(max(d$Y)+.1, 1))
    d
}
# --------------------------------------------
density_and_limits <- function( data ) {
  adj_density( data, 0, 0 )
}
# --------------------------------------------
sd_density <- function( data, height, mu ) {
    adj_density( data, height, mu )
}
# --------------------------------------------
plot_hist <- function( data, d, xlim, ylim, main ) {
  hist(data,probability=TRUE,col="lightgray",
       xlim=xlim,ylim=ylim, main=main )
  lines(d,col="blue")
}
# --------------------------------------------
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

plotbinom <- function( N, p, x, y, mu, sigma, xlim ) {
    # number of successes x in N trials
    
    plot(x,y,type="h",xlim=xlim,col="red",xlab='x',ylab='P(X=x)',main=sprintf( "Binomial (Number of Successes) N = %d, p = %6.2f, mu=%6.1f, sigma=%6.1f", N, p, mu,sigma ))
    show_mu_sigma(mu,sigma)
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
plotpoisson <- function( lambda, x, y, mu, sigma, xlim) {
    # Constant Rate of occurrence of events

    plot(x,y,type="h",xlim=xlim,col="red",xlab='x',ylab='P(X=x)',main=sprintf( "Poisson, lambda T = %8.4f, mu=%6.1f, sigma=%6.1f", lambda , mu, sigma))
    show_mu_sigma(mu,sigma)
}
# --------------------------------------------
# Binomial Distribution
# Negative Binomial Distribution
# Exponential Distribution
# Gamma Distribution
# Geometric Distribution
# Hypergeometric Distribution
# Beta Distribution
# Poisson Distribution
# Weibull Distribution
# Normal Distribution
# ---------------------------------------------------------------------------------------
show_mu_sigma <- function( mu, sigma ) {
  x <- c( mu-sigma, mu, mu+sigma)
  y <- c(0,0,0)
  points(x,y,pch=17,cex=2.5,col=c("magenta","darkred","magenta") )
  abline( v=mu,   col='darkred')
  abline( v=mu-sigma,col='magenta')
  abline( v=mu+sigma,col='magenta')
}
# ---------------------------------------------------------------------------------------
plotexponential <- function( x, y, mu, sigma, xlim ) {
    plot(x,y,type="b",pch='.',col="red",xlim=xlim,xlab='x',ylab='pdf(x)',
         main=substitute( paste( 'Exponential, ',
                                 lambda, " = ", l2, ",  ",
                                 mu,     " = ", m2, ",  ",
                                 sigma,  " = ", s2, 
                                 sep=""),
                          list( l2=round(lambdaT,0),m2=round(mu,2),s2=round(sigma,2)))
    )
    show_mu_sigma(mu,sigma)
}
# ---------------------------------------------------------------------------------------
plotgamma <- function( shape, rate, np=100, extent = 50 ) {
    x <- seq( from=0,to=extent, length.out=np )
    y <- dgamma(x, shape, rate )
    mu <- shape/rate
    sigma    <- sqrt(shape)/rate
    #plot(x,y,type="b",col="red",xlab='x',ylab='pdf(x)',
         #main=substitue(paste( "Gamma, shape r = %8.4f, rate lambda = %8.4f,     mu=%6.1f, sigma=%6.1f", shape, rate, mu, sigma )
    plot(x,y,type="b",col="red",xlab='x',ylab='pdf(x)',
         main=substitute(paste( 'Gamma, shape r = ', sh2, ", rate ",
                               lambda, " = ", r2, ", ",
                               mu,     " = ", m2, ", ",
                               sigma,  " = ", s2,
                               sep=''),
                        list( sh2=round(shape,2), r2=round(rate,2), m2=round(mu,2), s2=round(sigma,2) ))
    )
    show_mu_sigma(mu,sigma)
}
# ---------------------------------------------------------------------------------------
plotbeta <- function( shape1, shape2, np=101 ) {
    x <- seq( from=0,to=1, length.out=np )
    y <- dbeta(x, shape1, shape2 )
    
    s12 <- shape1+shape2
    mu  <- shape1/s12
    sigma    <- sqrt(shape1*shape2/(s12+1))/s12
    plot(x,y,type="b",col="red",xlab='x',ylab='pdf(x)',
         main=sprintf( "Beta s_1 = %8.4f, s_2 =%8.4f,   mu=%6.1f, sigma=%6.1f", shape1, shape2, mu, sigma ))
    show_mu_sigma(mu,sigma)
}
# ---------------------------------------------------------------------------------------
plotweibull <- function( shape, lambda, extent=20, np=101 ) {
    x <- seq( from=0,to=extent, length.out=np )
    y <- dweibull(x, shape, lambda )
    
    lg    <- gamma(1+1/shape)
    mu    <- lambda * lg
    sigma <- lambda* sqrt( gamma(1+2/shape) - lg*lg)
    plot(x,y,type="b",col="red",xlab='x',ylab='pdf(x)',
         main=sprintf( "Weibull beta = %8.4f, delta =%8.4f,   mu=%6.1f, sigma=%6.1f", shape, lambda, mu, sigma ))
    show_mu_sigma(mu,sigma)
}
# ---------------------------------------------------------------------------------------
plotnormal <- function( x,y, mu, sigma, xlim) {
    plot(x,y,type="b",xlim=xlim,col="red",xlab='x',ylab='pdf(x)',
         main=sprintf( "Gaussian, mu = %8.4f, sigma = %8.4f", mu, sigma ))
    show_mu_sigma(mu,sigma)
}
# ---------------------------------------------------------------------------------------
plotgaussianmixture <- function( x,y, mu, sigma, xlim) {
    plot(x,y,type="b",xlim=xlim,col="red",xlab='x',ylab='pdf(x)',
         main=sprintf( "Gaussian Mixture, mu = %8.4f, sigma = %8.4f", mu, sigma ))
    show_mu_sigma(mu,sigma)
}
# ---------------------------------------------------------------------------------------
plotcauchy <- function( x,y, xlim) {
    plot(x,y,type="b",xlim=xlim,col="red",xlab='x',ylab='pdf(x)',
         main="Cauchy, mu = Inf, sigma = Inf; CLT does not apply!")
}
# ---------------------------------------------------------------------------------------
plotlognormal <- function( meanlog, sdlog, np=50, ext_sigma=25) {
    med    <- exp(meanlog)
    mu     <- exp(meanlog+0.5*sdlog^2)
    sd2    <- exp(2*meanlog+sdlog^2)*(exp(sdlog^2)-1)
    sigma  <- sqrt(sd2)

    extent <- ext_sigma*sdlog

    x <- seq( 0.00001,to=extent,length=np )
    y <- dlnorm(x, meanlog, sdlog )

    plot(x,y,type="b",col="red",xlab='x',ylab='pdf(x)',
         main=sprintf( "Lognormal, theta=%6.2f, omega=%6.2f, median=%6.2f, mu = %6.2f, sigma = %6.2f", meanlog, sdlog, med, mu, sigma ))
    show_mu_sigma(mu,sigma)
}
# ---------------------------------------------------------------------------------------
plotuniform <- function( x, y, mu, sigma, xlim ) {
    plot(x,y,type="b",xlim=xlim,col="red",xlab='x',ylab='pdf(x)',
         main=sprintf( "Uniform, mu = %8.4f, sigma = %8.4f", mu, sigma ))
    show_mu_sigma(mu,sigma)
}

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
plotexponential <- function( lambda, np=100, extent=50) {
    x     <- seq( from=0,to=extent, length.out=np )
    y     <- dexp(x, lambda)
    mu    <- 1/lambda
    sigma <- 1/lambda

    #plot(x,y,type="b",pch='.',col="red",xlab='x',ylab='pdf(x)',main=sprintf( "Exponential Distr., lambda = %8.4f, mu=%6.1f, sigma=%6.1f", lambda,mu,sigma ))
    plot(x,y,type="b",pch='.',col="red",xlab='x',ylab='pdf(x)',
         main=substitute( paste( 'Exponential, ',
                                 lambda, " = ", l2, ",  ",
                                 mu,     " = ", m2, ",  ",
                                 sigma,  " = ", s2, 
                                 sep=""),
                          list( l2=round(lambda,2),m2=round(mu,2),s2=round(sigma,2)))
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
plotnormal <- function( mu, sigma, np=100, ext_sigma=3) {
    extent <- ext_sigma*ext_sigma
    x      <- seq( from=(-extent),to=extent,length=np )
    y      <- dnorm(x, mu, sigma )

    plot(x,y,type="b",col="red",xlab='x',ylab='pdf(x)',
         main=sprintf( "Gaussian, mu = %8.4f, sigma = %8.4f", mu, sigma ))
    show_mu_sigma(mu,sigma)
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

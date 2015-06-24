density_and_limits <- function( data ) {
  d      <- density(data)
  d$xlim <- c( round(min(data)-.3,1), round(max(data+.3),1))
  d$ylim <- c(0, round(max(d$y)+.1,1))
  d
}
# --------------------------------------------
sd_density <- function( data, height, mu ) {
    d   <- density(data)
    d$x <- d$x + mu
    d$y <- height * d$y / max(d$y)
    d
}
# --------------------------------------------
plot_hist <- function( data, d, xlim, ylim, main ) {
  hist(data,probability=TRUE,col="lightgray",
       xlim=xlim,ylim=ylim, main=main )
  lines(d,col="blue")
}
# --------------------------------------------
plot_coverage <- function( N, true_value, point_estimates, alpha, sigma, estimated=FALSE) {
  M       <- length( point_estimates )
  if (estimated == TRUE) {
      # case sigma is estimated
      err     <- qt(1-0.5*alpha, df=N-1) * sigma / sqrt(N)
  } else {
      # case sigma is known
      err     <- qnorm(1-0.5*alpha) * sigma / sqrt(N)
  }


  bound.l <- point_estimates - err
  bound.r <- point_estimates + err

  in_range  <- bound.l <= true_value & true_value <= bound.r
  
  coverage         <- sum(in_range)
  percent_coverage <- round(100*coverage/M,2)

  col       <- rep("red", M); col[in_range] <- "black"
  

  if (estimated == TRUE) {
      err.l <- round(min(err),2)
      err.m <- round(mean(err),2)
      err.h <- round(max(err),2)
      s     <- substitute( paste( 'Percentage of intervals that actually contain ',
                            mu,     " is ", percent_coverage, "%",
                            ", err = (", err.l,",",err.m,",",err.h,")",
                            sep=""))
  } else {
      err1  <- round(err,2)
      s     <- substitute( paste( 'Percentage of intervals that actually contain ',
                            mu,     " is ", percent_coverage, "%",
                            ", err = ", err1,
                            sep=""))
  }

  plot( range(min(bound.l),max(bound.r)), c(0, 1 + M), type = "n", # xlim=c(llim,rlim),
        xlab = "confidence interval (cm)", ylab = "sample run", main=s)
  
  for (i in 1:M) lines( range(c(bound.l[i],bound.r[i])), rep(i,2),lwd=2,col=col[i])
  points( point_estimates, 1:M,pch=19,col="magenta")
  abline(v = true_value, lwd = 2, lty = 2)
}
# --------------------------------------------
feathers      <- c(6.1, 6.5, 6.9, 7.5, 7.6, 7.7, 8.2, 8.3, 8.3, 8.6, 8.7,
                 8.8, 8.9, 9.1, 9.2, 9.3, 9.4, 9.8, 9.9, 10.1, 10.5, 10.9,
                 11.4, 11.6)

feathers   <- round(mean(feathers),1)
feathers   <- round(sd( feathers ),1)

feathers <- density_and_limits(feathers)

feathers <- feathers$xlim
feathers <- feathers$ylim

M           <- 100   # modify this only if the coverage plot is high enough...

###########################################################################################
# Material for later: https://en.wikipedia.org/wiki/Prediction_interval
#   case unknown mean, known variance:
#            (x_{n+1}-x_bar) / sqrt{1+1/n}   is approx N(0,1)
#   case known mean, unknown variance
#       (n-1) s^2_n / \sigma^2 \approx chi_squared(df=n-1)
#       so x_{n+1} \approx s student-t(df=n-1)
#   case unknown mean, unknown variance
#        T_a s_n sqrt(1+1/n)


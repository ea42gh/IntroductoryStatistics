#  population <- get(population, mode = "function")
# ==============================================================================================
plot_sample_estimates <- function(f_sample, num_samples, estimator=mean, num_experiments=300,title="Histogram", ...) {

  # define a vector to hold our sample estimates
  estimates <- double(num_experiments)

  # generate num_experiments samples of size num_samples and store the derived estimate
  for(i in 1:num_experiments) estimates[i] = estimator(f_sample(num_samples,...))

  # scale sample estimates to plot against standard normal
  scaled_estimates <- scale(estimates)

  # set up a two panel plot
  par(mfrow=c(1,2))
  par(mar=c(5,2,5,1)+0.1)

  # plot histogram and density of scaled estimates
  hist( scaled_estimates, prob=T, col="light grey", border="grey", main=NULL) #, ylim=c(0,0.4))
  lines(density(scaled_estimates),col="blue")

  # overlay the standard normal curve in red for comparison
  curve(dnorm(x,0,1), -3, 3, col='red', add=T)

  # adjust margins and draw the quantile-quantile plot
  par(mar=c(5,1,5,2)+0.1)
  qqnorm(estimates, main="")

  # return margins to normal and go back to one panel
  par(mar=c(5,4,4,2)+0.1)
  par(mfrow=c(1,1))

  # add a title
  par(omi=c(0,0,0.75,0))
  title(paste(title, ", n=", num_samples, sep=""), outer=T)
  par(omi=c(0,0,0,0))

  # return unscaled estimates (without printing)
  return(invisible(estimates))
}

if (F) {
   # show that as the sample size increases, the estimates approach a normal distibution
   x11(); plot_sample_estimates(runif, num_samples=1, title="Sample estimates from uniform distribution")
   x11(); plot_sample_estimates(runif, num_samples=2, title="Sample estimates from uniform distribution")
   x11(); plot_sample_estimates(runif, num_samples=10, title="Sample estimates from uniform distribution")
   
   # other distributions
   x11(); plot_sample_estimates(rexp, num_samples=6, title="Sample estimates from the exponential distribution", rate=1)
   x11(); plot_sample_estimates(rexp, num_samples=48, title="Sample estimates from the exponential distribution", rate=1)
} 

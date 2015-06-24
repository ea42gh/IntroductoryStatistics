library(shiny)
source('helper.R')
  #function() plothyper( 20, 80, 10, FALSE )
  #function() plotgeom( .5, 10 )
  #function() plotnbinom( 2, 10, .5 )

# ===================================================================
shinyServer(function(input, output) {
# -------------------------------------------------------------------
get_parent_distribution_info <- reactive({
  draw  <- function(v) plotpoisson( lambdaT, c(1,12), 1:12 )
  if      ( input$dist == "Normal Distribution"     ) { p <- def_normal()     }
  else if ( input$dist == "Gaussian Mixture"        ) { p <- def_gaussianmixture()    }
  else if ( input$dist == "Poisson Distribution"    ) { p <- def_poisson()    }
  else if ( input$dist == "Binomial Distribution"   ) { p <- def_binomial()   }
  else if ( input$dist == "Cauchy Distribution"     ) { p <- def_cauchy()     }
  else if ( input$dist == "Exponential Distrib."    ) { p <- def_exponential()}
  else if ( input$dist == "Uniform Distribution"    ) { p <- def_uniform()    }
  else                                                { p <- def_normal()     }
  p
})
# -------------------------------------------------------------------
get_statistic_function <- reactive({
  if      ( input$stat == "Mean"    ) { func2 <- mean   }
  else if ( input$stat == "Median"  ) { func2 <- median }
  else if ( input$stat == "IQR"     ) { func2 <- IQR    }
  else if ( input$stat == "Std Dev" ) { func2 <- sd     }
  else if ( input$stat == "MAX"     ) { func2 <- max    }
  else                                { func2 <- mean   }
  func2
})
# -------------------------------------------------------------------
output$parent_distribution <- renderPlot({
  p <- get_parent_distribution_info()
  #hist( p$data, probability = TRUE, main = input$dist, xlab="x" )
  p$draw()
})
# -------------------------------------------------------------------
output$sampling_distribution <- renderPlot({
  a    <- input$again
  dist <- input$dist

  p <- get_parent_distribution_info()
  s <- get_statistic_function()

  M <- as.integer(input$M)
  N <- as.integer(input$N)

  # get M random samples of size N
  samples <- p$sampling_function( N*M )
  samples <- matrix( samples, nrow=M ) # reorganize in matrix form

  # compute the statistic for each random sample
  theta_hat <- apply( samples, 1, s )

  # compute the density, adjust it's height for visibility
  h<-hist( theta_hat, probability = TRUE, xlim=p$xlim,plot=FALSE)
  h<-max(h$density)

  d         <- adj_density( theta_hat, h )

  m_val     <- round( mean(theta_hat), 3)
  sd_val    <- round( sd(theta_hat)*sqrt(N), 3)
  main      <- sprintf("%s, %s, mu_hat = %10.3f, sigma_hat * sqrt(N) = %10.3f", input$dist, input$stat, m_val, sd_val)
  
  # display histogram
  if (input$same_xlim == TRUE) {
      hist( theta_hat, probability = TRUE, main = main, xlab="x", xlim=p$xlim )
  } else {
      hist( theta_hat, probability = TRUE, main = main, xlab="x", xlim=input$xlim )
  }
  lines(d$X, d$Y, col="blue")
})
# -------------------------------------------------------------------
})

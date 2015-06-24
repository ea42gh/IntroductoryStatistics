library(shiny)
source('helper.R')
# -------------------------------------------------------------------
shinyServer(function(input, output) {
# -------------------------------------------------------------------
get_dat <- reactive({
    a      <- input$again
    N      <- input$N
    dat    <- matrix(sample(feathers,N*M,replace=TRUE),nrow=M)
    dat
})
# -------------------------------------------------------------------
get_muhat <- reactive({
    apply( get_dat(), 1, mean )
})
# -------------------------------------------------------------------
get_sd <- reactive({
    apply( get_dat(), 1, sd )
})
# -------------------------------------------------------------------
get_muhat_density <- reactive({
    mu_hat     <- get_muhat()
    muhat_dens <- density_and_limits(mu_hat)
    muhat_dens
})
# -------------------------------------------------------------------
output$parent_distribution <- renderPlot({
    if (input$estimated) {
        par(mfrow=c(1,2))
    }
    d    <- get_muhat_density()
    d$y  <-  1.5*max(feathers.dens$y)*d$y/max(d$y)
    ylim <-  c(0, round(max(feathers.ylim[2], max(d$y))+.1,1))

    vmu    <- feathers.mu
    vsd    <- feathers.sd
    main=substitute( paste( 'Lizard Tail Lengths, ',
                        mu,     " = ", vmu, ",  ",
                        sigma,  " = ", vsd,
                        ", scaled estimate of the mean in red",
                        sep=""))
    plot_hist(feathers, feathers.dens, feathers.xlim, ylim, main )
    lines(d, col="red")

    if (input$estimated) {
        muhat_sd <- get_sd()/sqrt(input$N)
        dens     <- density(muhat_sd)
        hist( muhat_sd, probability=TRUE, main='Standard Deviation of the Estimated Means')
        lines(dens,col="red")
    }
})
# -------------------------------------------------------------------
#output$point_estimate_distribution <- renderPlot({
#    plot_hist( get_muhat(), get_muhat_density(), feathers.xlim, 'Estimate of Mean Lizard Tail Length')
#})
# -------------------------------------------------------------------
output$coverage <- renderPlot({
    mh   <- get_muhat()
    sdmh <- get_sd()

    if (input$sorted) {
        o    <- order(mh)
        mh   <- mh[o]
        sdmh <- sdmh[o]
    }

    if (input$estimated) {
        plot_coverage( input$N, feathers.mu, mh, input$alpha, sdmh, input$estimated )
    } else {
        plot_coverage( input$N, feathers.mu, mh, input$alpha, feathers.sd, input$estimated )
    }
})
# -------------------------------------------------------------------
})

# server.R

# ============================================================================================
shinyServer(function(input, output, clientData, session ) {
  # ==========================================================================================
  output$tbl    <- renderDataTable ({ data.frame(faithful$eruptions) },  options = list(iDisplayLength = 10))

  # eruption duration versus index
  output$plot1a <- renderPlot      ({ plot(faithful$eruptions,ylab="Duration of Eruptions (min)",col="blue") })
  output$plot1b <- renderPlot      ({ plot(faithful$eruptions,ylab="Duration of Eruptions (min)",col="blue", type="b") })
  output$plot1c <- renderPlot      ({ plot(faithful$eruptions,ylab="Duration of Eruptions (min)",col="blue", type="h") })

  # eruption duration versus index, sorted by magnitude
  output$plot2  <- renderPlot      ({ plot(sort(faithful$eruptions),xlab="Observations sorted by magnitude", ylab="Duration of Eruptions (min)",col="blue") })

  # number of eruptions of a given duration versus duration 
  output$plot3  <- renderPlot      ({ hist(faithful$eruptions,breaks=length(faithful$eruptions),freq=TRUE,xlab="Duration of Eruptions (min)") })
  output$text3a <- renderPrint     ({ stem(faithful$eruptions) })
  output$plot4  <- renderPlot      ({ hist(faithful$eruptions,breaks=16,freq=TRUE,col="lightblue",xlab="Duration of Eruptions (min)") })

  # data summary: boxplot
  output$text5  <- renderPrint     ({ summary(data.frame(faithful$eruptions)) })
  output$plot5  <- renderPlot      ({ boxplot(faithful$eruptions,col="lightblue",horizontal=TRUE,notch=TRUE,xlab="Duration of eruptions (min)") })
})

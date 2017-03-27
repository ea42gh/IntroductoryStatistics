library(shiny)
source('helper.R')
# Define UI for random distribution application
shinyUI(fluidPage(
mainPanel(
   fluidRow(
     column(3,sliderInput("N", label = "Average of N values",
                min = 2, max = 3000, value = 4, step = 1)),
     column(3,sliderInput("alpha", label = "alpha",
              min = 0.001, max = 0.1, value = 0.05, step = 0.001)),
     column(3,checkboxInput( "estimated", label = "Check if Sigma is Estimated"),value=FALSE),
     column(3,checkboxInput( "sorted", label = "Sort Intervals"),value=FALSE),
     column(3,actionButton( "again", label = "Repeat"))
   )

  , plotOutput("parent_distribution")
  #, plotOutput("point_estimate_distribution")
  , plotOutput("coverage")
)))

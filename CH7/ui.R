library(shiny)
source('helper.R')
# Define UI for random distribution application
shinyUI(fluidPage(
mainPanel(
   fluidRow(
     column(3,sliderInput("N", label = "Number of Experiments N",
                animate=animationOptions(interval=500),
                min = 1, max = 484, value = 4, step = 1, round=TRUE)),
     column(3,sliderInput("M", label = "Number of Runs M",
              min = 1, max = 3000, value = 10, step = 1, round=TRUE)),

     column(3,selectInput( "dist", label = "Distribution: ",
                           choices = c( "Normal Distribution",
                                        "Gaussian Mixture",
                                        "Poisson Distribution",
                                        "Binomial Distribution",
                                        "Cauchy Distribution",
                                        "Exponential Distrib.",
                                        "Uniform Distribution"
                                        ), selected = "Normal Distribution" )),

     column(3,selectInput( "stat", label = "Statistic",
                           choices = c("Mean", "Median", "IQR", "MAX", "Std Dev" ), selected = "Mean")),
     column(5,sliderInput("xlim", label = "x limits",
              min=-30, max=30, value = c(-3,3) )),

     column(2,checkboxInput( "same_xlim", label = "Same x scale", TRUE )),
     column(3,actionButton( "again", label = "Repeat"))
   ),

   plotOutput("parent_distribution"),
   plotOutput("sampling_distribution")
)))


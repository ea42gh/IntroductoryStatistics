library(shiny)

shinyUI(fluidPage(
   #titlePanel("Data Collection"),
   
   mainPanel( #h1("Data Collection"),
                tabsetPanel( tabPanel("Table",   dataTableOutput('tbl'))
                            ,tabPanel("Plot_1a", plotOutput('plot1a'))
                            ,tabPanel("Plot_1b", plotOutput('plot1b'), plotOutput('plot1c'))
                            ,tabPanel("Plot_2",  plotOutput('plot2'))
                            ,tabPanel("Plot_3",  verbatimTextOutput('text3a'), plotOutput('plot3'))
                            ,tabPanel("Plot_4",  plotOutput('plot4'))
                            ,tabPanel("Plot_5",  verbatimTextOutput('text5'),  plotOutput('plot5'))
                , width="100%" )
     )
))

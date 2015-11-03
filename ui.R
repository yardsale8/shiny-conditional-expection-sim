library(shiny)

shinyUI(fluidPage(
  titlePanel("Conditional Expectation Simulation"),
 sidebarLayout(
  sidebarPanel(
         sliderInput("p", label = h3("Probability of Success"), min = 0, 
                     max = 1, value = 0.5),
  verbatimTextOutput("p"),
  checkboxGroupInput("size", "Sample Size", 
                     choices = c(1, 10, 100, 1000, 10000, 100000), 
                     selected=1),
  actionButton("drawSample", label = "Draw a Sample"),
  actionButton("reset", label = "Reset")
  ),
 
 mainPanel(plotOutput("plot"))
)))
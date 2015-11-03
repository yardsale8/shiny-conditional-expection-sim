library(shiny)

source("utilities.R")

shinyServer(function(input, output) {
  output$p <- renderPrint({ input$p })
  output$size <- renderPrint({input$size})

  # A data structure to store the state of the simulation
  vals = reactiveValues(counts = reset_summaries())
  
  observe({
    a <- input$p
    b <- input$reset
    vals$counts <- reset_summaries()
  })
  
  
  #vals$counts <- isolate({update_counts(vals$counts, get_summaries(get_trials(input$size, input$p)))})
    output$plot <-renderPlot({
      a <- input$drawSample
      b <- input$reset
      vals$counts <- isolate(update_counts(vals$counts, input$size, input$p))
      isolate(plot_summaries(input$p, vals$counts))})
})
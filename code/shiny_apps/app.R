source("plot_functions.R")
wavenumbers = readRDS("wavenumbers.rds")
data = read.csv(file = "reference_database.csv", header = TRUE)
library(shiny)
shinyApp(

  ui = fluidPage(
    selectInput("class", "Class:",
                choices = unique(data$class)),
    plotOutput("meanSpectrumPlot")
  ),

  server = function(input, output) {
    output$meanSpectrumPlot = renderPlot({
      meanplot(data, wavenumbers = wavenumbers, class = input$class)
    })
  },

  options = list(height = 500)
)

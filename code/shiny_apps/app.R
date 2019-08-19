source("plot_functions.R")
wavenumbers = readRDS("wavenumbers.rds")
data = read.csv(file = "reference_database.csv", header = TRUE)
library(shiny)
library(plotly)
library(ggplot2)
shinyApp(

  ui = fluidPage(
    selectInput("class", "Class:",
                choices = unique(data$class)),
    plotlyOutput("meanSpectrumPlot")
  ),

  server = function(input, output) {
    output$meanSpectrumPlot = renderPlotly({
      meanplot(data, wavenumbers = wavenumbers, class = input$class)

    })
  },

  options = list(height = 500)
)

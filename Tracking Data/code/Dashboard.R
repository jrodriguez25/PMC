library(shinydashboard)
library(shiny)
library(tidyverse)
library(lubirdate)
library(plotly)





# Load Data ---------------------------------------------------------------

clean_tracking <- read_csv("clean_tracking.csv")

clean_tracking


# Wranling continued ------------------------------------------------------







# View App ----------------------------------------------------------------

ui <- dashboardPage(
  
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody( fluidRow(
    box(plotOutput("plot1", height = 250)),
    
    box(
      title = "Controls",
      sliderInput("slider", "Number of observations:", 1, 100, 50)
    )
  )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)


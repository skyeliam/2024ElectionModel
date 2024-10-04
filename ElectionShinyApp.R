library(shiny)
library(tidyverse)

stateOptions <- c(state.name,"Maine CD-2","Maine CD-2","Nebraska CD-2")
ui <- fluidPage(
  htmlOutput("header"),
  plotOutput("ECHist"),
  plotOutput("PopVsEC"),
  selectInput("statePick", "State/district level statistics:", choices = stateOptions,selected="Pennsylvania"),
  htmlOutput("summary"),
  #plotOutput("plot")
)

server <- function(input, output, session) {
  output$header <- renderText({
    paste0("<h1>Welcome to Liam's 2024 Presidential Election Simulator</h1>")
  })
  state <- reactive({
    ElectionSims[,input$statePick]
  })
  output$ECHist <- renderPlot({
    histGenerator()
  }, res = 96)
  output$PopVsEC <- renderPlot({
    dotplotGenerator()
  }, res = 96)
  output$summary <- renderText({
    winProb <- sum(state()>0)/4000
    outputString <- paste0("Harris has a ",sprintf("%.2f%%",winProb*100)," chance of winning ",input$statePick)
    outputString <- paste0(outputString, "<p>","Her average margin in ",input$statePick, " is ", sprintf("%.2f%%",mean(state())),"</p>")
    outputString
  })
}

shinyApp(ui, server)
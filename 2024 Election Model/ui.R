#a shiny app for displaying the results of the election simulation
library(shiny)
library(tidyverse)
library(shinydashboard)


#for the drop down menu to get state level data
stateOptions <- c(state.name,"Maine CD 2","Maine CD 2","Nebraska CD 2")

#defining UI
ui <- dashboardPage(skin = "purple",
                    dashboardHeader(
                      title = "Liam's 2024 Election Nowcast",
                      titleWidth = "calc(100% - 44px)"),
                    dashboardSidebar(disable=TRUE),
                    dashboardBody(
                      htmlOutput("header"),
                      fluidRow(tabItem(tabName = "Election Plots",box(plotOutput("ECHist",height = "400px"),
                                                             plotOutput("forestPlot",height="800px")),
                              box(selectInput("statePick", "State/district level statistics:", 
                                              choices = stateOptions,selected="Pennsylvania"),
                                  plotOutput("stateForest",height="200px"),
                                  htmlOutput("summary")),
                              box(plotOutput("PopVsEC"),width=12))),
                      htmlOutput("footer")
                    ))
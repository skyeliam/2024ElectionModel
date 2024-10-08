#a shiny app for displaying the results of the election simulation
library(shiny)
library(tidyverse)
library(shinydashboard)
library(DescTools)

latestSimData <- read.csv(url("https://raw.githubusercontent.com/skyeliam/2024ElectionModel/refs/heads/main/Latest_Election%20Simulation%20Data.csv"))
colnames(latestSimData) <- str_replace_all(colnames(latestSimData),"\\."," ")
timeStamp <- StrExtractBetween(read_file(url(
  "https://raw.githubusercontent.com/skyeliam/2024ElectionModel/refs/heads/main/timestamp.txt"))," ","\n")
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
                      fluidRow(tabItem(tabName = "Election Plots",box(
                        title = p(strong(paste("Harris wins",sprintf("%.1f%%",sum(latestSimData$ElectoralVotes>=270)/40),"of simulations"))),
                                                                      plotOutput("ECHist",height = "400px"),
                                                             plotOutput("forestPlot",height="800px")),
                              box(selectInput("statePick", "State/district level statistics:", 
                                              choices = stateOptions,selected="Pennsylvania"),
                                  plotOutput("stateForest",height="200px"),
                                  htmlOutput("summary")),
                              box(plotOutput("PopVsEC"),width=12,
                                  title = p(strong("Popular vote margin vs. Electoral College margin"))))),
                      htmlOutput("footer")
                    ))
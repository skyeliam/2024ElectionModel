# run.R
library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(shinydashboard)
library(DescTools)
port <- Sys.getenv('PORT')
shiny::runApp(
  appDir = getwd(),
  host = '0.0.0.0',
  port = as.numeric(port)
)
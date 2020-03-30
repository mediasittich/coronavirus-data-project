library(shiny)
library(coronavirus)
library(leaflet)
library(DT)

source("myUI.R", local = TRUE)
source("myServer.R")

shinyApp(
  ui = myUI,
  server = myServer
)
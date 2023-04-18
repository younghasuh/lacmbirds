# ui.R

library(shiny)


shinyUI(fluidPage(
  
  titlePanel("Specimen trends and maps"),
  
  textInput("sp", "Species"),
  
  fluidRow(
    column(12, plotOutput("trend"))
  ),
  
  fluidRow(
    column(12, plotOutput("spp"))
  ),
  
  fluidRow(
    column(12, plotOutput("state"))
  ),
  
  fluidRow(
    column(12, plotOutput("county"))
  ),
  
  fluidRow(
    column(2, tableOutput("countbyyear")),
    column(3, tableOutput("summary"))
  )
  
  
))
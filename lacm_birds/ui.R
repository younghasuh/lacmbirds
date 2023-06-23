# ui.R

library(shiny)


shinyUI(fluidPage(
  
  titlePanel("LACM Specimen trends and map"),
  
  textInput("sp", "Species"),
  
  fluidRow(
    column(12, tableOutput("specnat"))
  ),
  
  
  fluidRow(
    column(12, plotOutput("trend"))
  ),
  
  # fluidRow(
  #   column(12, plotOutput("spp"))
  # ),
  
  fluidRow(
    column(12, plotOutput("state"))
  ),
  
  fluidRow(
    column(12, plotOutput("ca_cty"))
  ),
  
  
  fluidRow(
    column(2, tableOutput("countbyyear")),
    column(3, tableOutput("summary"))
  )
  
  
))
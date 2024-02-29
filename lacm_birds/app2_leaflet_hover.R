# version to add leaflet map from app2.R

library(shiny)
library(ggplot2)
library(tidyverse)
library(urbnmapr)
library(usmap)
library(here)
library(leaflet)
library(sf)
library(htmltools)

# set up wd
setwd("~/lacmbirds/lacm_birds")
here::i_am("app2.R")

# load data
data5 <- read.csv("data.csv")
specnat <- read.csv(here("specnat.csv"))
sta <- read.csv(here("states.csv"))


## App testing grounds ## 

# Define UI for application that draws a histogram
ui <- fluidPage(tags$head(tags$style(HTML('* {font-family: "Arial"};'))),
  mainPanel(
    tabsetPanel(
      type = "tabs",
      # tab 1
      tabPanel(
        titlePanel("Species summary"),
        
        sidebarLayout(
          sidebarPanel(
        textInput("sp", "Species"),
        fluidRow(column(12, h4("Specimen count by specimen type/nature"), tableOutput("specnat")))),
        
        mainPanel(
        
      fluidRow(column(12, h4("Global specimen distribution"), leafletOutput(outputId = 'map')))
    ))))))


server <- function(input, output, session) {
  
  selected <- reactive(data5 %>% filter(species == input$sp))
  
  output$specnat <- renderTable(
    selected() %>% count(Description))
  
  map_df <- reactive({
   selected() %>% 
      filter(!is.na(lng) & !is.na(lat)) %>% 
      st_as_sf(coords = c("lng", "lat"))
  })
  
  # output$map = renderLeaflet({
  #   leaflet() %>%
  #     addTiles() %>%
  #     addCircleMarkers(data = map_df(), radius=1) 
  # })
  
  # different approach
    # output$map = renderLeaflet({
    #   leaflet() %>%
    #     addTiles() %>%
    #     addCircleMarkers(data = map_df(), radius=1, popup=~htmlEscape(paste("LACM ",lacm, "<br>", datecoll, sep = " ")))
    #     
    #  })
    
    output$map = renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addCircleMarkers(data = map_df(), radius=1, 
                         popup=paste("LACM ", map_df()$lacm, "<br>", map_df()$datecoll, sep = " "))
      
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

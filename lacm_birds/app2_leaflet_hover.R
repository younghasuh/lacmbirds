# version to add leaflet map from app2.R

library(shiny)
library(ggplot2)
library(tidyverse)
library(urbnmapr)
library(usmap)
library(here)
library(leaflet)
library(sf)


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
        
        fluidRow(column(12, h4("Specimen count by year"), plotOutput("trend"))),
        
        fluidRow(column(12, h4("Global specimen distribution"), leafletOutput(outputId = 'map')))
    ))))))


server <- function(input, output, session) {
  
  selected <- reactive(data5 %>% filter(species == input$sp))
  
  output$countbyyear <- renderTable(
    selected() %>% count(year))
  
  output$summary <- renderTable(
    selected() %>% count(year, specnat))
  
  output$specnat <- renderTable(
    selected() %>% count(Description))
  
  trend1 <- reactive({
    selected() %>% 
      filter(specnat == "SS" | specnat == "SN")})
  
  output$trend <- renderPlot({
    trend1() %>% 
      ggplot(aes(x = year, fill = specnat, color = specnat)) +
      geom_bar(position = position_dodge(preserve = "single")) +
      scale_x_continuous(breaks = seq(1880, 2020, 10)) +
      xlim(1850, 2023) +
      theme_classic() +
      labs(fill = "Specimen type", color = "Specimen type", x = "Year", y = "Count")
    
  }, res = 96)
  
  
  
  #selected <- reactive(data5 %>% filter(species == input$sp))
  map_df <- reactive({
   selected() %>% 
      filter(!is.na(lng) & !is.na(lat)) %>% 
      st_as_sf(coords = c("lng", "lat"))
  })
  
  output$map = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = map_df(), radius=1) 
  
      
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

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
        
        fluidRow(column(12, h4("Specimen count by month"), plotOutput("trend2"))),
        
        fluidRow(column(12, h4("Specimen count by state (US only)"), plotOutput("state"))),
        
        fluidRow(column(12, h4("Specimen count by county (CA only)"), plotOutput("ca_cty"))),
        
        fluidRow(column(12, h4("Global specimen distribution"), leafletOutput(outputId = 'map')))
        
        ))
      ),
      
      # tab 2      
      tabPanel(
        titlePanel("LACM lookup"),
        textInput("catalog", "LACM"),
        fluidRow(column(12, tableOutput("catsum"))),
                 
#        fluidRow(column(12, plotOutput("specmap"))),
                 
        fluidRow(column(12, h4("Leaftlet map"), leafletOutput(outputId = 'catmap')))
        ),
      
      
      # tab 3
      tabPanel(
        titlePanel("Specimen type"),
        textInput("spc", "Species"),
        fluidRow(column(3,  radioButtons("spectype", h3("Specimen type:"),
                                         choices = c("Study skins" = "ss", "Skeleton" = "sk",
                                                        "All" = "all"), selected = "ss")),
                 column(12, tableOutput("speclist")))
      )
    )
  )
)

server <- function(input, output, session) {
  
  selected <- reactive(data5 %>% filter(species == input$sp))
  
  output$countbyyear <- renderTable(
    selected() %>% count(year)
  )
  
  
  output$summary <- renderTable(
    selected() %>% count(year, specnat)
  )
  
  output$specnat <- renderTable(
    selected() %>% count(Description)
  )
  
  trend1 <- reactive({
    selected() %>% 
      filter(specnat == "SS" | specnat == "SN")
  })
  
  output$trend <- renderPlot({
    trend1() %>% 
      ggplot(aes(x = year, fill = specnat, color = specnat)) +
      geom_bar(position = position_dodge(preserve = "single")) +
      scale_x_continuous(breaks = seq(1880, 2020, 10)) +
      xlim(1850, 2023) +
      theme_classic() +
      labs(fill = "Specimen type", color = "Specimen type", x = "Year", y = "Count")
    
  }, res = 96)
  
  # count by month and type
  output$trend2 <- renderPlot({
    trend1() %>% 
      ggplot(aes(x = month, fill = specnat, color = specnat)) +
      geom_bar(position = position_dodge(preserve = "single")) +
     scale_x_continuous(breaks = seq(1, 12, 1), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
      theme_classic() +
      labs(fill = "Specimen type", color = "Specimen type", x = "Month", y = "Count")
    
  }, res = 96)
  
  # reactive map by state
  spat_state1 <- reactive({
    left_join(get_urbn_map(map = "states", sf = TRUE),
              selected() %>% 
                count(abv),
                by = c("state_abbv" = "abv"))
  })
  
  # app format  
  output$state <- renderPlot({
    spat_state1() %>% 
      ggplot() +
      geom_sf(spat_state1(),
              mapping = aes(fill = n),
              color = "#ffffff", size = 0.25) +
      labs(fill = "Specimen count") +
      scale_fill_viridis_c(option = "D")  
  })
  
  
  # reactive map by county 
  # within California only  
  spat_ca_cty <- reactive({
    left_join(get_urbn_map(map = "counties", sf = TRUE) %>% 
              filter(state_abbv == "CA"),
              selected() %>% 
              count(cty2),
              by=c("county_name"="cty2")) 
  })


  # app format  
  output$ca_cty <- renderPlot({
    spat_ca_cty () %>% 
      ggplot() +
      geom_sf(spat_ca_cty(),
              mapping = aes(fill = n),
              color = "#ffffff", size = 0.25) +
      labs(fill = "Specimen count") +
      scale_fill_viridis_c(option = "D")  
  })
  
  
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
  
  
  ##### Tab 2
  selected2 <- reactive(data5 %>% filter(lacm == input$catalog)) 
  
  output$catsum <- renderTable(
    selected2() %>% 
      mutate(
        LACM = lacm,
        LAF = laf,
        Family = family,
        Species = species,
        Subspecies = spp,
        Sex = sex,
        Date = datecoll,
        Locality = locality
      ) %>% 
      select(LACM, LAF, Family, Species, Subspecies, Sex, Date, Description, Locality)
  )
  
  
 # mapdat <- reactive({
#    usmap_transform(selected2(), input_names = c("lng", "lat"))
#  }) # cannot derive nonnumeric; need to remove NA for lat/long 
  
#  output$specmap <- renderPlot({
#    plot_usmap("states") +
#      geom_point(data = mapdat(),
#                 aes(x=x, y=y), color="red", size=3)
#  })
  
  # using leaflet instead
  catmap_df <- reactive({
    selected2() %>% 
      filter(!is.na(lng) & !is.na(lat)) %>% 
      st_as_sf(coords = c("lng", "lat"))
  })
  
  output$catmap = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = catmap_df(), radius=1)
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

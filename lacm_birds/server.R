# server.R

library(shiny)
library(ggplot2)
library(tidyverse)
library(urbnmapr)

data <- read.csv("data.csv")

shinyServer(function(input, output, session) {
  
  selected <- reactive(data %>% filter(species == input$sp))
  
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
      filter(specnat == "SS" | specnat == "SN" | specnat == "SW")
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
  
  
  # output$spp <- renderPlot({
  #   selected() %>% 
  #     ggplot(aes(x = year, fill = spp, color = spp)) +
  #     geom_histogram(breaks = seq(1880, 2020, by = 10), alpha = 0.5, position="dodge2") +
  #     scale_x_continuous(breaks = seq(1880, 2020, 10)) +
  #     theme_classic() +
  #     labs(fill = "Subspecies", color = "Subspecies", x = "Year", y = "Count")
  # }, res = 96)
  
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
  
  
})
  
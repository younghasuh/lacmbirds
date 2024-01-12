# server.R

library(shiny)
library(ggplot2)
library(tidyverse)
library(urbnmapr)
library(here)
library(usmap)

data <- read.csv("data_2023.csv")
autocomplist <- data$species
specnat <- read.csv(here("specnat.csv"))
sta <- read.csv(here("states.csv"))

shinyServer(function(input, output, session) {
  
  updateSelectizeInput(session, "sp", choices = autocomplist, selected=character(0), server = TRUE)
  
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
      labs(fill = "Specimen type", color = "Specimen type", x = "Year", y = "Count", title = "Specimen count by year")
    
  }, res = 96)
  
  # count by month and type
  output$trend2 <- renderPlot({
    trend1() %>% 
      ggplot(aes(x = month, fill = specnat, color = specnat)) +
      geom_bar(position = position_dodge(preserve = "single")) +
      scale_x_continuous(breaks = seq(1, 12, 1), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
      theme_classic() +
      labs(fill = "Specimen type", color = "Specimen type", x = "Month", y = "Count", title = "Specimen count by month")
    
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
      labs(fill = "Specimen count", title = "Specimen count by State") +
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
      labs(fill = "Specimen count", title = "Specimen count by County") +
      scale_fill_viridis_c(option = "D")  
  })
  
  ##### Tab 2
  selected2 <- reactive(data %>% filter(lacm == input$catalog)) 
  
  output$catcount <- renderTable(
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
  
  
  
  mapdat <- reactive({
    usmap_transform(selected2(), input_names = c("lng", "lat"))
  }) # cannot derive nonnumeric; need to remove NA for lat/long 

  output$specmap <- renderPlot({
    plot_usmap("states") +
      geom_point(data = mapdat(),
                 aes(x=x, y=y), color="red", size=3)
  })
  
  
  #############
  # tab 3
  
  selected3 <- reactive(data %>% filter(species == input$sp))
  
  #input$spectype == ss, sk, all
  
  output$speclist <- renderTable(
    selected3() %>% 
      filter(species == input$spc,
             specnat == ifelse(input$spectype == "ss", "SS"))
  )
  
  
})
  
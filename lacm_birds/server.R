# server.R

library(plotly)
library(shiny)
library(ggplot2)
library(tidyverse)
library(urbnmapr)
library(here)
library(usmap)
library(leaflet)
library(sf)
library(ggiraph)
library(htmltools)
library(shinydashboard)
library(DT)


# Load data
data <- read.csv("data_20240301.csv")
alist <- sort(unique(unlist(data$species, use.names = FALSE)))
ind <- c("sex", "spp", "state", "month")


shinyServer(function(input, output, session) {
  
  ### TAB 1
  # using selectize input; putting autofill list on server side to reduce processing speed
  updateSelectizeInput(session, "sp", choices = alist, selected=character(0), server = TRUE)
  
  # reactive input for species for tab 1
  selected <- reactive(data %>% filter(species == req(input$sp)))
  
  
  # table for specimen nature 
  output$specnat <- renderTable(
    selected() %>% count(Description)
  )
  
  # table for sex
  output$sexcount <- renderTable(
    selected() %>% count(Sex)
  )
  
  # table for age
  output$agecount <- renderTable(
    selected() %>% count(age)
  )
  
  # table for list
  output$spectab <- renderDT({
    dat <- selected() %>% 
      mutate(
        LACM = lacm,
        LAF = laf,
        Family = family,
        Species = species,
        Subspecies = spp,
        Date = datecoll,
        Locality = locality
      ) %>% 
      select(LACM, LAF, Family, Species, Subspecies, Sex, Date, Description, Locality)
    
    DT::datatable(dat,
                  class = 'cell-border stripe',
                  rownames = F,
                  extensions = c("Buttons", "Select"),
                  selection = 'none',
                  options = 
                    list(
                      pageLength = 10, autoWidth = TRUE,
                      dom = 'Bfrtip',
                      select = TRUE,
                      buttons = list(
                        list(
                          extend = "copy",
                          text = 'Copy'#,
                          #exportOptions = list(modifier = list(selected = TRUE))
                        )
                      )
                    )) 
  })
  
  
  # filter only skeletons and study skins for simplified figures
  data_filt <- reactive({
    selected() %>% 
      filter(nat == "skeleton" | nat == "study skin")
  })
  
  output$trend <- renderPlot({
    data_filt() %>% 
      ggplot(aes(x = year, fill = nat, color = nat)) +
      geom_bar(position = position_dodge(preserve = "single")) +
      scale_x_continuous(breaks = seq(1880, 2020, 10)) +
      xlim(1850, 2023) +
      theme_classic() +
      labs(fill = "Specimen type", color = "Specimen type", x = "Year", y = "Count")
    
  }, res = 96)
  
  # count by month and type
  output$trend2 <- renderPlot({
    data_filt() %>% 
      ggplot(aes(x = month, fill = nat, color = nat)) +
      geom_bar(position = position_dodge(preserve = "single")) +
      scale_x_continuous(breaks = seq(1, 12, 1), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
      theme_classic() +
      labs(fill = "Specimen type", color = "Specimen type", x = "Month", y = "Count")
    
  }, res = 96)
  
  
  
  # reactive map by state
  # US only
  spat_state1 <- reactive({
    left_join(get_urbn_map(map = "states", sf = TRUE),
              selected() %>% 
                count(abv),
              by = c("state_abbv" = "abv"))
  })
  
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
  
  
  output$ca_cty <- renderPlot({
    spat_ca_cty () %>% 
      ggplot() +
      geom_sf(spat_ca_cty(),
              mapping = aes(fill = n),
              color = "#ffffff", size = 0.25) +
      labs(fill = "Specimen count") +
      scale_fill_viridis_c(option = "D")  
  })
  
  
  # leaflet map
  # global
  map_df <- reactive({
    selected() %>% 
      filter(!is.na(lng) & !is.na(lat)) %>% 
      st_as_sf(coords = c("lng", "lat"))
  })
  
  output$map = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = map_df(), radius=1, 
                       popup=paste("LACM ", map_df()$lacm, "<br>", map_df()$datecoll, sep = " ")) 
  })  
  
  ## tab 2 
  # boxplot for weights
  output$wtPlot <- renderPlot({
    selected() %>% 
      ggplot(aes(x=Sex, y=wt)) +
      stat_boxplot(geom="errorbar", position="dodge2") +
      geom_boxplot(stat = "boxplot",
                   position = "dodge2") + 
      geom_point(shape=16, alpha=0.4, position=position_jitter(0.2)) +
      theme_minimal() +
      scale_x_discrete(limits = c("M", "F", "U"), labels = c("Male", "Female", "Unknown")) +
      labs(x = "Sex", y = "Weight (g)")
  })
  
  ## tab 3
  # with ggiraph for interactive plot 
  selected_pts <- reactive({
    input$wtPlot2_selected
  })
  
  output$console <- renderPrint({
    input$wtPlot2_hovered
  })
  
  output$wtPlot2 <- renderGirafe({
    gg_bx <- ggplot(selected(), aes(x=Sex, y=wt)) +
      stat_boxplot(geom="errorbar", position="dodge2") +
      geom_boxplot(stat="boxplot", position="dodge2", outlier.shape = NA) +
      geom_point_interactive(aes(tooltip=lacm, data_id=lacm),
                             size=3, hover_nearest=T, position=position_jitter(0.2))  +
      scale_x_discrete(limits = c("M", "F", "U"), labels = c("Male", "Female", "Unknown")) +
      labs(x = "Sex", y = "Weight (g)")
    girafe(ggobj = gg_bx)
  })
  
  output$datatab <- renderTable({
    #  selected_pts() 
    out <- selected()[selected()$lacm %in% selected_pts(),] %>% 
      mutate(LACM = lacm, LAF = laf, subspecies = spp, Date = datecoll, Locality = locality, SpecType = nat) %>% 
      select(LACM, LAF, Sex, subspecies, Date, Locality, SpecType) 
    if( nrow(out) < 1 ) return(NULL)
    row.names(out) <- NULL
    out
  })
  
  ## tab 4
  # plotly boxplots - variable y
   x <- reactive({
     x <- selected()[[input$xaxis]]
   })
   
   output$plot <- renderPlotly({
     dat <- selected()
     plot_ly(dat, x = x(), y = dat$wt, type = "box",
             boxpoints = "all", jitter = 0.8,
             pointpos = 0, marker = list(size = 3),
             hoverinfo = "text",
             text = ~paste("LACM:", dat$lacm, ";", "Weight:", dat$wt, sep=" "))  %>%
       layout(boxmode = "group",
              xaxis = list(title='Grouping'),
              yaxis = list(title='Weight (g)'))
   })
  
  
  #### TAB 2 ----
  selected2 <- reactive(data %>% filter(lacm == input$catalog)) 
  
  output$catcount <- renderTable(
    selected2() %>% 
      mutate(
        LACM = lacm,
        LAF = laf,
        Family = family,
        Species = species,
        Subspecies = spp,
        Date = datecoll,
        Locality = locality
      ) %>% 
      select(LACM, LAF, Family, Species, Subspecies, Sex, Date, Description, Locality)
  )
  
  
  catmap_df <- reactive({
    selected2() %>% 
      filter(!is.na(lng) & !is.na(lat)) %>% 
      st_as_sf(coords = c("lng", "lat"))
  })
  
  output$catmap = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = catmap_df(), radius=1, 
                       popup=paste("LACM ", catmap_df()$lacm, "<br>", catmap_df()$datecoll, sep = " "))
    
  })
  
  
  #### TAB 3 ----
  output$lacmsumm <- renderTable(
    max(data$lacm),
    rownames = F, colnames = F
  )
  
  output$toptable <- renderDT({
    gettab <- data %>% count(get(input$category))
    
    DT::datatable(gettab,
                  class = 'cell-border stripe',
                  rownames = F,
                  colnames = c("", "N"))
  })
  
  
})
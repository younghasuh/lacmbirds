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
library(plotly)
library(shinydashboard)
library(DT)
library(rclipboard)

setwd("C:/Users/young/Documents/lacmbirds/lacm_birds")

here::i_am("lacm_birds/app_joint.R")

md2 <- read.csv("merged_data2.csv")
md2$date <- as.Date(md2$date)


# test <- md2[which(md2$species == "cumv/Eremophila alpestris"),]


alist <- sort(unique(unlist(md2$species, use.names = FALSE)))

ui <- shinyUI(
  
  fluidPage(
    
    titlePanel(title =  div(img(src="NHM_logo_black_250.png", width="50px", height="50px"), 
                            "Ornithology Collections"), windowTitle = "Ornithology Collections"),
    
    tabsetPanel(
      type = "tabs",
      
      # tab 1 ----
      tabPanel(
        titlePanel("Species summary"),
        
        sidebarLayout(
          sidebarPanel(
            h2("Species"),
            p("Start typing species and select from drop-down list."),
            
            selectizeInput(
              inputId = 'sp',
              label = 'Species',
              choices = NULL,
              selected = NULL,
              multiple = FALSE, # allow for multiple inputs
              options = NULL)),
          
          # main panel for displaying outputs for the selectize Input (species name)
          mainPanel(
            
            tabsetPanel(type = "tabs",
                        # sub tab 1
                        tabPanel(title = "Summary", 
                                 fluidRow(column(12, h4("Specimen count by specimen type/nature"), tableOutput("specnat"))),
                                 fluidRow(column(6, h4("By sex", tableOutput("sexcount"))))),
                        # sub tab 2
                        tabPanel(title = "Count figures", 
                                 fluidRow(column(12, h4("Specimen count by year"), plotOutput("trend"))),
                                 fluidRow(column(12, h4("Specimen count by month"), plotOutput("trend2"))),
                                 fluidRow(column(12, h4("Specimen count by state (US only)"), plotOutput("state"))),
                                 fluidRow(column(12, h4("Specimen count by county"), plotOutput("county"))),
                                 fluidRow(column(12, h4("Global specimen distribution"), leafletOutput(outputId = 'map')))
                        ),
                        # sub tab 3
                        tabPanel(title = "Data explorationg with Plotly",
                                 fluidRow(column(12, h4("Plotly boxplots"), 
                                                 box(selectInput("xaxis", "Select independent variable (x-axis)",
                                                                 choices = c("Sex", "state", "month"))))),
                                 fluidRow(column(12, plotlyOutput("plot")))
                        ),
                        # sub tab 4
                        tabPanel(title = "Table of all specimens",
                                 fluidRow(column(12, h4("List of all specimens. Use shift or ctrl to select multiple rows for copying onto clipboard."),
                                                 DTOutput("spectab"))))
            )
          ) # mainpanel end
        ) # sidebar layout end
      ), # tabPanel end
      
      # tab 2 ----
      # start new tabPanel --
      tabPanel(
        titlePanel("Catalog lookup"),
        selectInput(inputId = "collx", label = "Collection", choices = c("LACM", "CUMV")),
        textInput("catnum", "Catalog number:"),
        fluidRow(column(12, h5("Examples: use CUMV - 10078, LACM - 121077"), tableOutput("catcount"))),
        fluidRow(column(12, h4("Leaftlet map"), leafletOutput(outputId = 'catmap')))
      ),
      
      # tab 3 ----
      tabPanel(
        titlePanel("Compare species"),
        selectizeInput(inputId = 'sp1', label = 'Species 1', choices = NULL, selected = NULL, multiple = FALSE, options = NULL),
        selectizeInput(inputId = 'sp2', label = 'Species 2', choices = NULL, selected = NULL, multiple = FALSE, options = NULL),
        plotlyOutput("plot_spcomp")
      ),
      
      # tab 4 ----
      tabPanel(
        titlePanel("Explore the collection"),
        fluidRow(h4("Last updated XX XXX XXXX")),
        fluidRow(column(12, h4("Total number of specimens"), tableOutput("summ"))),
        fluidRow(column(12, selectInput("category", "Select category:",
                                        choices = c("Sex", "family", "genus", "year", "country", "state", "county")))),
        fluidRow(column(12, tableOutput("toptable"))),
        fluidRow(column(12, tableOutput("toptable2")))
      )
      
    )
  )
)



server <- shinyServer(function(input, output, session) {
  
  
  #### TAB 1. SPECIES SUMMARY ----

  # using selectize input; putting autofill list on server side to reduce processing speed
  updateSelectizeInput(session, "sp", choices = alist, selected=character(0), server = TRUE)
  
  # reactive input for species for tab 1
  selected <- reactive(md2 %>% filter(species == req(input$sp)))
  
  ### sub tab 1. Summary ----
  # table for specimen nature 
  output$specnat <- renderTable(
    selected() %>% 
      group_by(collection) %>% count(skin, skeleton, ethanol)
  )
  
  
  # table for sex
  output$sexcount <- renderTable(
    selected() %>% 
      group_by(collection) %>% count(Sex)
  )
  
  
  ### sub tab 2. Count figures ----
  
  # filter only skeletons and study skins for simplified figures
  data_filt_yr <- reactive({
    selected() %>% 
      count(decade, skin, skeleton) %>% 
      pivot_longer(cols=c(skin, skeleton), names_to = "nat", values_to = "count") %>% na.omit()
  })
  
  # mdl <- md %>% 
  #   count(year, skin, skeleton) 
  # 
  # mdl2 <- mdl %>% 
  #   pivot_longer(cols=c(skin, skeleton), names_to = "nat", values_to = "count") %>% na.omit()
  #   
  # ggplot(mdl2, aes(x = year, y = n, fill = nat)) + 
  #   geom_bar(stat = "identity",position = "dodge") +
  #   xlim(1850, 2023)
  
  output$trend <- renderPlot({
    data_filt_yr() %>% 
      ggplot(aes(x = decade, y = n, fill = nat)) +
      geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
      scale_x_continuous(breaks = seq(1880, 2020, 10)) +
      xlim(1850, 2023) +
      theme_classic() +
      labs(fill = "Specimen type", color = "Specimen type", x = "Year", y = "Count")
    
  }, res = 96)
  
  
  
  # count by month and type
  data_filt_mo <- reactive({
    selected() %>% 
      count(month, skin, skeleton) %>% 
      pivot_longer(cols=c(skin, skeleton), names_to = "nat", values_to = "count") %>% na.omit()
  })
  
  output$trend2 <- renderPlot({
    data_filt_mo() %>% 
      ggplot(aes(x = month, fill = nat, color = nat)) +
      geom_bar(position = position_dodge(preserve = "single")) +
      scale_x_continuous(breaks = seq(1, 12, 1), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
      theme_classic() +
      labs(fill = "Specimen type", color = "Specimen type", x = "Month", y = "Count")
    
  }, res = 96)
  
  
  # reactive map by state (US only)
  spat_state1 <- reactive({
    left_join(get_urbn_map(map = "states", sf = TRUE),
              selected() %>% 
                count(state),
              by = c("state_name" = "state"))
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
  
  
  # reactive map by county (US only)
  spat_county <- reactive({
    left_join(get_urbn_map(map = "counties", sf = TRUE),
              selected() %>% count(county),
              by = c("county_name" = "county"))
  })
  
  output$county <- renderPlot({
    spat_county() %>% 
      ggplot() +
      geom_sf(spat_county(),
              mapping = aes(fill = n),
              color = "#ffffff", size = 0.25) +
      labs(fill = "Specimen count") +
      scale_fill_viridis_c(option = "D")  
  })
  
  
  # leaflet map (global)
  map_df <- reactive({
    selected() %>% 
      filter(!is.na(lng) & !is.na(lat)) %>% 
      st_as_sf(coords = c("lng", "lat"))
  })
  
  output$map = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = map_df(), radius=1, 
                       popup=paste(map_df()$name, "<br>", map_df()$date, sep = " ")) 
  })  
  
  
  ### sub tab 3. Explore data with plotly ---- 
  
  x <- reactive({
    x <- selected()[[input$xaxis]]
  })
  
  
  output$plot <- renderPlotly({
    dat <- selected()
    plot_ly(dat, x = x(), y = dat$wt, type = "box",
            boxpoints = "all", jitter = 0.8,
            pointpos = 0, marker = list(size = 5),
            hoverinfo = "text",
            text = ~paste(dat$name, ";", "Weight:", dat$wt, sep=" "))  %>%
      layout(boxmode = "group",
             xaxis = list(title='Grouping'),
             yaxis = list(title='Weight (g)'))
  })
  
  ### sub tab 4. table of all specimens ---- 
  
  # table for list
  output$spectab <- DT::renderDT({
    dat <- selected() %>% 
      mutate(
        Catalog = catalog,
        Family = family,
        Species = species,
        Date = date,
        Locality = locality,
        Collection = collection
      ) %>% 
      select(Collection, Catalog, Family, Species, Sex, Date, Locality)
    
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
                          text = 'Copy'
                        )
                      )
                    ))
  })

  
  
  #### TAB 2 ---- 
  # add a drop down to select collection then search by catalog #
  selected_cat <- reactive({
    req(input$collx)
    req(input$catnum)
    md2 %>% 
      dplyr::filter(collection %in% input$collx & catalog %in% input$catnum)
  })
  
  output$catcount <- renderTable(
    selected_cat() %>% 
      mutate(
        Catalog = catalog,
        Family = family,
        Species = species,
        Date = as.character(date),
        Locality = locality
      ) %>% 
      select(Catalog, Family, Species, Sex, Date, Locality)
  )
  
  
  # using leaflet
  catmap_df <- reactive({
    selected_cat() %>% 
      filter(!is.na(lng) & !is.na(lat)) %>% 
      st_as_sf(coords = c("lng", "lat"))
  })
  
  output$catmap = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = catmap_df(), radius=1, 
                       popup=paste(catmap_df()$name, "<br>", "Date:", as.character(catmap_df()$date)))
    
  })
  
  
  #### TAB 3 ---
  updateSelectizeInput(session, "sp1", choices = alist, selected=character(0), server = TRUE)
  updateSelectizeInput(session, "sp2", choices = alist, selected=character(0), server = TRUE)
  
  selected3 <- reactive(md2 %>% filter(species == req(input$sp1)))
  selected4 <- reactive(md2 %>% filter(species == req(input$sp2)))
  
  output$plot_spcomp <- renderPlotly({
    dat3 <- selected3()
    dat4 <- selected4()
    plot_ly(dat3, x = dat3$Sex, y = dat3$wt, type = "box", boxmean = T, name = dat3$species,
            boxpoints = "all", jitter = 0.8,
            pointpos = 0, marker = list(size = 5))  %>%
      add_trace(dat4, x = dat4$Sex, y = dat4$wt, name = dat4$species) %>% 
      layout(boxmode = "group",
             xaxis = list(title='Grouping'),
             yaxis = list(title='Weight (g)'))
  })
  
  
  #### TAB 4 ---- not edited yet
  
  # tabPanel(
  #   titlePanel("Explore the collection"),
  #   fluidRow(h4("Last updated XX XXX XXXX")),
  #   fluidRow(column(12, h4("Total number of specimens"), tableOutput("summ"))),
  #   fluidRow(column(12, selectInput("category", "Select category:",
  #                                   choices = c("Sex", "family", "genus", "year", "country", "state", "county")))),
  #   fluidRow(column(12, tableOutput("toptable"))),
  #   fluidRow(column(12, DTOutput("toptable2")))
  # )
  
  output$summ <- renderTable(
    md2 %>% 
      group_by(collection) %>% 
      summarise(N = n()),
    rownames = F, colnames = F
  )
  
  
  output$toptable <- renderTable(
    md2 %>% 
      group_by(collection) %>% 
      count(get(input$category))
  )
  
  output$toptable2 <- renderTable(
    md2 %>% 
      group_by(get(input$category)) %>% 
      count(collection)
  )
  
})


shinyApp(ui = ui, server = server)


### issues
# no description for cumv
#

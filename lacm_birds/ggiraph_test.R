## current issue
# none

## current goals
# make barplots and/or map interactive
# make leaflet map interactive 

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

#####
# UI
ui <- shinyUI(fluidPage(
  
  #titlePanel("LACM Specimen trends and maps"),
  titlePanel(title =  div(img(src="NHM_logo_black_250.png", width="50px", height="50px"), 
                          "LACM Specimen trends and maps"), windowTitle = "LACM Specimen trends and maps"),
  
  mainPanel(
    tabsetPanel(
      type = "tabs",
      
      # tab 1
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
              options = NULL),
            
            
            fluidRow(column(12, h4("Specimen count by specimen type/nature"), tableOutput("specnat")))
          ),
          
          mainPanel(
            
            fluidRow(column(12, h4("Specimen count by year"), plotOutput("trend"))),
            
            fluidRow(column(12, h4("Specimen count by month"), plotOutput("trend2"))),
            
            fluidRow(column(12, h4("Specimen count by state (US only)"), plotOutput("state"))),
            
            fluidRow(column(12, h4("Specimen count by county (CA only)"), plotOutput("ca_cty"))),
            
            fluidRow(column(12, h4("Global specimen distribution"), leafletOutput(outputId = 'map'))),
            
            fluidRow(column(12, h4("Weights by sex"), plotOutput(outputId = "wtPlot"))),
            
            fluidRow(
              column(2, tableOutput("countbyyear")),
              column(3, tableOutput("summary")))
          )
        )),
      
# tab 2
      tabPanel(
        titlePanel("LACM lookup"),
        textInput("catalog", "LACM"),
        fluidRow(column(12, tableOutput("catcount"))),
        fluidRow(column(12, h4("Leaftlet map"), leafletOutput(outputId = 'catmap')))
      ),

# tab 3
    tabPanel(
      titlePanel("Species summary - detailed plots"),
      sidebarLayout(
        sidebarPanel(
          h2("Species"),
          p("Start typing species and select from drop-down list."),
          
          selectizeInput(
            inputId = 'sp2',
            label = 'Species',
            choices = NULL,
            selected = NULL,
            multiple = FALSE, # allow for multiple inputs
            options = NULL)),
        
        mainPanel(
          fluidRow(column(8, h4("Weights by sex - interactive"), girafeOutput("wtPlot2")),
                   column(4, h4("Hovering points"), verbatimTextOutput("console"),
                          h4("Selected points"), tableOutput("datatab"))),
          
        )
      )
    )))
))

#####

server <- shinyServer(function(input, output, session) {
  
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
  
  # leaflet map
  map_df <- reactive({
    selected() %>% 
      filter(!is.na(lng) & !is.na(lat)) %>% 
      st_as_sf(coords = c("lng", "lat"))
  })
  
 
  labs <- as.list(selected()$lacm)
  
  output$map = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = map_df(), radius=1, label=lapply(labs, HTML)) 
  })  
  
  # boxplot for weights
  output$wtPlot <- renderPlot({
    selected() %>% 
      ggplot(aes(x=sex, y=wt)) +
      stat_boxplot(geom="errorbar", position="dodge2") +
      geom_boxplot(stat = "boxplot",
                   position = "dodge2") + 
      geom_point(shape=16, alpha=0.4, position=position_jitter(0.2)) +
      theme_minimal() +
      scale_x_discrete(limits = c("M", "F", "U"), labels = c("Male", "Female", "Unknown")) +
      labs(x = "Sex", y = "Weight (g)")
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
  
  
  #############
  # tab 3
  # with ggiraph for interactive plot  
  
  updateSelectizeInput(session, "sp2", choices = autocomplist, selected=character(0), server = TRUE)
  
  selected2 <- reactive(data %>% filter(species == input$sp2))
  
  selected_pts <- reactive({
    input$wtPlot2_selected
  })
  
  output$console <- renderPrint({
    input$wtPlot2_hovered
  })
  
  
  output$wtPlot2 <- renderGirafe({
    gg_bx <- ggplot(selected2(), aes(x=sex, y=wt)) +
      stat_boxplot(geom="errorbar", position="dodge2") +
      geom_boxplot(stat="boxplot", position="dodge2", outlier.shape = NA) +
      geom_point_interactive(aes(tooltip=lacm, data_id=lacm),
                             size=3, hover_nearest=T, position=position_jitter(0.2))  +
      scale_x_discrete(limits = c("M", "F", "U"), labels = c("Male", "Female", "Unknown")) +
      labs(x = "Sex", y = "Weight (g)")
    girafe(ggobj = gg_bx, 
           width_svg = 6, height_svg = 5,
           options = list(
             opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;", reactive = TRUE),
             opts_selection(
               type = "multiple", css = "fill:#FF3333;stroke:black;")))
  })
  
  
  observeEvent(input$reset, {
    session$sendCustomMessage(type = 'plot_set', message = character(0))
  })
  
  
  output$datatab <- renderTable({
    #  selected_pts() 
    out <- selected2()[selected2()$lacm %in% selected_pts(),] %>% 
      mutate(LACM = lacm, LAF = laf, Sex = sex, subspecies = spp, Date = datecoll, Locality = locality, SpecType = nat) %>% 
      select(LACM, LAF, Sex, subspecies, Date, Locality, SpecType) 
    if( nrow(out) < 1 ) return(NULL)
    row.names(out) <- NULL
    out
  })
  
  
  
})


shinyApp(ui = ui, server = server)


# Interactive Shiny plot - weight by species/sex


library(shiny)
library(ggplot2)
library(tidyverse)
library(urbnmapr)
library(usmap)
library(here)
library(leaflet)
library(sf)
library(ggiraph)

# set up wd
setwd("~/lacmbirds/lacm_birds")
here::i_am("app3.R")

# load data
#data <- read.csv("sampledata.csv") # randomly sampled 1000 specimens from original database
data <- read.csv("data_2023.csv")

acl <- sort(unique(unlist(autocomplist, use.names = FALSE)))


#data0 <- data[-c(1, 1:5)]

#table(data$specnat)

#data$nat <- ifelse(data$specnat == "AL" | data$specnat == "AC" | data$specnat == "AO", "fluid", 
#                   ifelse(data$specnat == "SS" | data$specnat == "SW" | data$specnat == "SA" | data$specnat == "KB", "study skin",
#                          ifelse(data$specnat == "SN" | data$specnat == "SNB" | data$specnat == "SNW" | data$specnat == "SO", "skeleton",
#                                 "other")))

#table(data$nat)

#write.csv(data, "data_2023.csv")

test <- data[which(data$species == "Tyto alba"),]

g <- ggplot(test, aes(x=sex, y=wt))
my_gg <- g + geom_point_interactive(aes(tooltip=lacm, data_id=lacm),
                                    size=3, hover_nearest=T)
girafe(ggobj=my_gg)

# selected <- reactive(data %>% filter(species == input$sp))

# gg_bx <- ggplot(data, aes(x=sex, y=wt)) +
#  stat_boxplot(geom="errorbar", position="dodge2") +
#  geom_boxplot(stat="boxplot", position="dodge2") +
#  geom_point_interactive(aes(tooltip=lacm, data_id=lacm),
#                         size=3, hover_nearest=T)  +
#  scale_x_discrete(limits = c("M", "F", "U"), labels = c("Male", "Female", "Unknown")) +
#  labs(x = "Sex", y = "Weight (g)")
# girafe(ggobj = gg_bx)

# shiny ex
ui <- shinyUI( mainPanel(
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
          fluidRow(column(8, h4("Weight boxplot"), girafeOutput("plot")),
                   column(4, h4("Hovering points"), verbatimTextOutput("console"),
                                h4("Selected points"), tableOutput("datatab")))
          
        ))))))

server <- shinyServer(function(input, output, session) {
  
  updateSelectizeInput(session, "sp", choices = autocomplist, selected=character(0), server = TRUE)
  
  selected <- reactive(data %>% filter(species == input$sp))
  
  selected_pts <- reactive({
    input$plot_selected
  })
  
  output$console <- renderPrint({
    input$plot_hovered
  })
    
  output$plot <- renderGirafe({
    
    gg_bx <- ggplot(selected(), aes(x=sex, y=wt)) +
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
      mutate(LACM = lacm, LAF = laf, Sex = sex, subspecies = spp, Date = datecoll, Locality = locality, SpecType = nat, Weight = wt) %>% 
      select(LACM, LAF, Sex, subspecies, Date, Locality, SpecType, Weight) 
    if( nrow(out) < 1 ) return(NULL)
    row.names(out) <- NULL
    out
  })
  
  
})

shinyApp(ui = ui, server = server)



#####
# Define uI
ui <- fluidPage(
  
  titlePanel(title =  div(img(src="NHM_logo_black_250.png", width="50px", height="50px"), 'LACM Specimen trends and maps'), windowTitle =
    "LACM Specimen trends and maps"),
  
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
            fluidRow(column(12, h4("Weights by sex"), plotOutput(outputId = "wtPlot2"))),
            
            
            fluidRow(h4("Detailed counts by year"),
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
      )

)))

#####
# Define server
server <- function(input, output, session) {
  
  updateSelectizeInput(session, "sp", choices = acl, selected=character(0), server = TRUE)
  
  selected <- reactive(data %>% filter(species == input$sp))
  
  output$wtPlot <- renderPlot({
    selected() %>% 
    ggplot(aes(x=sex, y=wt.j)) +
      stat_boxplot(geom="errorbar", position="dodge2") +
      geom_boxplot(stat = "boxplot",
                   position = "dodge2") + 
      geom_point(shape=16, alpha=0.4) +
      theme_minimal() +
      scale_x_discrete(limits = c("M", "F", "U"), labels = c("Male", "Female", "Unknown")) +
      labs(x = "Sex", y = "Weight (g)")
  })
  
  #position=position_jitter(0.2), 
    
  output$wtdata <- renderTable({
    nearPoints(selected(), input$plot_click, allRows = F)
  }) # this works but doesn't want to maintain when there are 2+ points with the same y value
  
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
  
  output$countbyyear <- renderTable(
    selected() %>% count(year)
  )
  
  output$summary <- renderTable(
    selected() %>% count(year, specnat)
  )
  
  output$specnat <- renderTable(
    selected() %>% count(Description)
  )
  
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
  
  output$map = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = map_df(), radius=1) 
  })  
  
  # boxplots
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
  
  # make boxplots interactive with ggiraph

  
  output$wtPlot2 <- renderPlot({
    selected() %>% 
      ggplot(aes(x=sex, y=wt)) +
      stat_boxplot(geom="errorbar", position="dodge2") +
      geom_boxplot(stat = "boxplot",
                   position = "dodge2") + 
      geom_point_interactive(aes(tooltip=lacm, data_id=lacm), hover_nearest = TRUE, shape=16, alpha=0.4, position=position_jitter(0.2)) +
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
  
}

shinyApp(ui = ui, server = server)

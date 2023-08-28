library(shiny)
library(ggplot2)
library(tidyverse)
library(urbnmapr)
library(readxl)

testdat <- read_excel("C:/Users/ysuh/Documents/lacmbirds/test/C.cardinalis_Shiny.xlsx")
testdat$species <- paste(testdat$genus, testdat$specificEpithet, sep = " ")
testdat$countyname <- paste(testdat$county, "County")

ui <- fluidPage(
  
  titlePanel("Specimen trends and maps"),
  
  textInput("sp", "Species"),
  
  fluidRow(
    column(12, tableOutput("prep"))
  ),
  
  fluidRow(
    column(12, plotOutput("trend"))
  ),
  
  fluidRow(
    column(12, plotOutput("trend2"))
  ),

  fluidRow(
    column(12, plotOutput("state"))
  ),
  
  fluidRow(
    column(12, plotOutput("county"))
  ),
  
  fluidRow(
    column(12, plotOutput("pa_cty"))
  ),
  
  fluidRow(
    column(2, tableOutput("countbyyear")),
    column(3, tableOutput("summary"))
  )
  
)

server <- function(input, output, session) {
  
  selected <- reactive(testdat %>% filter(species == input$sp))
  
  output$countbyyear <- renderTable(
    selected() %>% count(year)
  )
  
  output$summary <- renderTable(
    selected() %>% count(year, Preparations)
  )
  
  output$prep <- renderTable(
    selected() %>% count(Preparations)
  )
  
  
  trend1 <- reactive({
    selected()
  })
  
  
  output$trend <- renderPlot({
    trend1() %>% 
      ggplot(aes(x = year, fill = Preparations, color = Preparations)) +
      geom_bar(position = position_dodge(preserve = "single")) +
      scale_x_continuous(breaks = seq(1880, 2020, 10)) +
      xlim(1850, 2023) +
      theme_classic() +
      labs(fill = "Specimen type", color = "Specimen type", x = "Year", y = "Count", title="All years") + 
      theme(plot.title = element_text(size=20, face="bold"))
    
  }, res = 96)
  
  output$trend2 <- renderPlot({
    trend1() %>% 
      ggplot(aes(x = year, fill = Preparations, color = Preparations)) +
      geom_bar(position = position_dodge(preserve = "single")) +
      scale_x_continuous(breaks = seq(2000, 2020, 2)) +
      xlim(2000, 2023) +
      theme_classic() +
      labs(fill = "Specimen type", color = "Specimen type", x = "Year", y = "Count", title="2000 and after") +
      theme(plot.title = element_text(size=20, face="bold"))
    
  }, res = 96)
  
  # map by state
  spat_state1 <- reactive({
    left_join(get_urbn_map(map = "states", sf = TRUE),
              selected() %>% 
                count(stateProvince),
              by = c("state_name" = "stateProvince"))
  })
  
  output$state <- renderPlot({
    spat_state1() %>% 
      ggplot() +
      geom_sf(spat_state1(),
              mapping = aes(fill = n),
              color = "#ffffff", size = 0.25) +
      labs(fill = "Specimen count") +
      scale_fill_viridis_c(option = "D")  +
      labs(title="Specimen count by state") + 
      theme(plot.title = element_text(size=20, face="bold"))
  })
  
  
  # map by county
  spat_cty1 <- reactive({
    left_join(get_urbn_map(map = "counties", sf = TRUE),
              selected() %>% 
                count(countyname),
              by = c("county_name" = "countyname"))
  })
  
  output$county <- renderPlot({
    spat_cty1() %>% 
      ggplot() +
      geom_sf(spat_cty1(),
              mapping = aes(fill = n),
              color = "#ffffff", size = 0.25) +
      labs(fill = "Specimen count") +
      scale_fill_viridis_c(option = "D")  +
      labs(title="Specimen count by county") + 
      theme(plot.title = element_text(size=20, face="bold"))
  })
  
  
  # within PA
  spat_pa_cty <- reactive({
    left_join(get_urbn_map(map = "counties", sf = TRUE) %>% 
                filter(state_abbv == "PA"),
              selected() %>% 
                count(countyname),
              by=c("county_name" = "countyname")) 
  })
  
  output$pa_cty <- renderPlot({
    spat_pa_cty () %>% 
      ggplot() +
      geom_sf(spat_pa_cty(),
              mapping = aes(fill = n),
              color = "#ffffff", size = 0.25) +
      labs(fill = "Specimen count") +
      scale_fill_viridis_c(option = "D")  +
      labs(title="Specimen count by PA counties") + 
      theme(plot.title = element_text(size=20, face="bold"))
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(ggplot2)
library(tidyverse)
library(urbnmapr)

setwd("~/birds/lacm_birds")

data <- read.csv("Birds_Collection.csv")

data <- data %>% 
  select(Catalog.No, Field.No, Sex, LAF.No, Age, Spec.Nat, Measurements, Gonads, Weight, Collector, Date.Coll, Family, Genus, Species, Subspecies, Continent, Country, State, County, Township, Nearest.Named.Place) %>% 
  mutate(lacm = Catalog.No,
         field = Field.No,
         sex = Sex,
         laf = LAF.No,
         age = Age, 
         specnat = Spec.Nat,
         measure = Measurements,
         gonads = Gonads,
         wt = as.numeric(Weight),
         coll = Collector,
         datecoll = Date.Coll,
         species = paste(Genus, Species, sep = " "),
         spp = Subspecies,
         genus = Genus,
         family = Family,
         locality = paste(Country, State, County, Township, Nearest.Named.Place, sep = " "),
         state = State,
         county = County
  ) %>% 
  select(lacm, field, sex, laf, age, specnat, measure, gonads, wt, coll, datecoll, family, species, genus, spp, locality, state, county)



# transform date into an actual date category
data$date <- as.Date(data$datecoll, format="%d %B %Y")

data$year <- as.numeric(format(data$date, "%Y"))
data$month <- as.numeric(format(data$date, "%m"))

# remove odd dates
data2 <- data %>% filter(date > "1800-01-01")

#write.csv(data2, "C:/Users/ysuh/Documents/birds/lacm_birds/data.csv", row.names=TRUE)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Specimen trends and maps"),
  
  textInput("sp", "Species"),
  
  fluidRow(
    column(12, plotOutput("trend"))
  ),
  
  fluidRow(
    column(12, plotOutput("spp"))
  ),
  
  fluidRow(
    column(12, plotOutput("state"))
  ),
  
  # fluidRow(
  #   column(12, plotOutput("county"))
  # ),
  
  fluidRow(
    column(2, tableOutput("countbyyear")),
    column(3, tableOutput("summary"))
  )
  
  
)

server <- function(input, output, session) {
  
  selected <- reactive(data2 %>% filter(species == input$sp))
  
  output$countbyyear <- renderTable(
    selected() %>% count(year)
  )
  
  output$summary <- renderTable(
    selected() %>% count(year, specnat)
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
      theme_classic() +
      labs(fill = "Specimen type", color = "Specimen type", x = "Year", y = "Count")
    
  }, res = 96)
  
  
  output$spp <- renderPlot({
    selected() %>% 
      ggplot(aes(x = year, fill = spp, color = spp)) +
      geom_histogram(breaks = seq(1880, 2020, by = 10), alpha = 0.5, position="dodge2") +
      scale_x_continuous(breaks = seq(1880, 2020, 10)) +
      theme_classic() +
      labs(fill = "Subspecies", color = "Subspecies", x = "Year", y = "Count")
  }, res = 96)
  
  
  # statecount1 <- reactive({
  #   selected() %>% 
  #     count(state) %>% 
  #     mutate(state2 = substr(state, 1, 2)) 
  # })
  
  spat_state1 <- reactive({
    left_join(get_urbn_map(map = "states", sf = TRUE),
              selected() %>% 
                count(state) %>% 
                mutate(state2 = substr(state, 1, 2)),
              by = c("state_abbv" = "state2"))
  })
   
  
  
  # app format  
  output$state <- renderPlot({
    spat_state1() %>% 
      ggplot() +
      geom_sf(spat_state1(),
              mapping = aes(fill = n),
              color = "#ffffff", size = 0.25) +
      labs(fill = "Specimen count")
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(ggplot2)
library(tidyverse)
library(urbnmapr)
library(here)

#install.packages("devtools")
#devtools::install_github("UrbanInstitute/urbnmapr")

#setwd("~/lacmbirds/lacm_birds")
here::i_am("app.R")

##########
# Run this step for every new download from EMu
#data <- read.csv("C:/Users/ysuh/Documents/lacmbirds/lacm_birds/Birds_Collection.csv")

data <- read.csv(here("Birds_Collection.csv"))
specnat <- read.csv(here("specnat.csv"))
sta <- read.csv(here("states.csv"))


data2 <- data %>% 
  select(Catalog.No, Field.No, Sex, LAF.No, Age, Spec.Nat, Measurements, Gonads, Weight, Collector, Date.Coll, Family, Genus, Species, Subspecies, Continent, Country, State, County, Township, Nearest.Named.Place,
         Elevation, Latitude.Dec, Longitude.Dec) %>% 
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
         county = County,
         ele = Elevation,
         lat = Latitude.Dec,
         lng = Longitude.Dec
  ) %>% 
  select(lacm, field, sex, laf, age, specnat, measure, gonads, wt, coll, datecoll, family, species, genus, spp, locality, state, county, ele, lat, lng)




# transform date into an actual date category
data2$date <- as.Date(data2$datecoll, format="%d %B %Y")

data2$year <- as.numeric(format(data2$date, "%Y"))
data2$month <- as.numeric(format(data2$date, "%m"))

# remove odd dates
data3 <- data2 %>% filter(date > "1800-01-01" & date < "2100-01-01")

# for specnat, combine with spelled out types
data4 <- data3 %>% 
  left_join(specnat, by="specnat")

# add state abbreviations
data5 <- data4 %>% 
  left_join(sta, by = c("state" = "full"))

# change county input 
## county is in all caps -> change to just first letter cap only to match urbn county map
data5$cty <- str_to_title(data5$county) 
data5$cty2 <- gsub(" Co", " County", data5$cty)
# I used to merge State with County since there are duplicate counties
# But since I'm only doing CA, I can skip that step. Different issue if doing whole country. 

#write.csv(data5, here("data.csv"), row.names=TRUE)

##########



## App testing grounds ## 

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Specimen trends and maps"),
  
  textInput("sp", "Species"),
  
  fluidRow(
    column(12, tableOutput("specnat"))
  ),
  
  
  fluidRow(
    column(12, plotOutput("trend"))
  ),
  
  
  # fluidRow(
  #   column(12, plotOutput("spp"))
  # ),
  
  fluidRow(
    column(12, plotOutput("state"))
  ),
  
   fluidRow(
     column(12, plotOutput("ca_cty"))
   ),
  
  
  fluidRow(
    column(2, tableOutput("countbyyear")),
    column(3, tableOutput("summary"))
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
  
  
  # output$spp <- renderPlot({
  #   selected() %>% 
  #     ggplot(aes(x = year, fill = spp, color = spp)) +
  #     geom_histogram(breaks = seq(1880, 2020, by = 10), alpha = 0.5, position="dodge2") +
  #     scale_x_continuous(breaks = seq(1880, 2020, 10)) +
  #     theme_classic() +
  #     labs(fill = "Subspecies", color = "Subspecies", x = "Year", y = "Count")
  # }, res = 96)
  # 
  
  ### this part is merged with below
  # statecount1 <- reactive({
  #   selected() %>% 
  #     count(state) %>% 
  #     mutate(state2 = substr(state, 1, 2)) 
  # })

  # original  
  # spat_state1 <- reactive({
  #   left_join(get_urbn_map(map = "states", sf = TRUE),
  #             selected() %>% 
  #               count(state) %>% 
  #               mutate(state2 = substr(state, 1, 2)),
  #             by = c("state_abbv" = "state2"))
  # })
  
  
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
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(ggplot2)
library(tidyverse)
library(urbnmapr)
library(here)
library(usmap)

#
setwd("~/lacmbirds/lacm_birds")
here::i_am("app2.R")

##########
# Run this step for every new download from EMu
#data <- read.csv("C:/Users/ysuh/Documents/lacmbirds/lacm_birds/Birds_Collection.csv")

data <- read.csv(here("Birds_Collection.csv"))
data1 <- read.csv(here("birds_2023.csv"))

specnat <- read.csv(here("specnat.csv"))
sta <- read.csv(here("states.csv"))

# new data flat file from Oct 2023
data2 <- data1 %>% 
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


data6 <- data5 %>% 
  mutate(spectype_sp = ifelse(specnat == "SS" | specnat == "SW", "Round skin", 
                           ifelse(specnat == "FB", "Flat skin and skeleton",
                                  ifelse(specnat == "KB", "Round kin and skeleton",
                                         ifelse(specnat == "AL", "Fluid",
                                  ifelse(specnat == "SN" | specnat == "SNB" | specnat == "SO" |
                                           specnat == "SNNW" | specnat == "SNT", "Skeleton", NA))))))




write.csv(data5, here("data_2023.csv"), row.names=TRUE)

data5 <- read.csv("data.csv")

##########



## App testing grounds ## 

# Define UI for application that draws a histogram
ui <- fluidPage(
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel(
        titlePanel("Specimen type"),
        textInput("spc", "Species"),
        fluidRow(column(3,  radioButtons("spectype", h3("Specimen type:"),
                                         choices = c("Study skins" = "ss", "Skeleton" = "sk",
                                                     "Fluid" = "al",
                                                     "All" = "all"), selected = "ss")),
                 column(12, tableOutput("speclist")))
      )
    )
  )
)

server <- function(input, output, session) {
  
  selected3 <- reactive(data6 %>% filter(species == input$sp))
  
  #input$spectype == ss, sk, al, all
  
  output$speclist <- renderTable(
    selected3() %>% 
      switch(spectype_sp == ifelse(input$spectype == "ss", "Study skin", NA))
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

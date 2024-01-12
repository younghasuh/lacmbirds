# copied app 2 to test autofill -> works! 1/11/2014
# using server-side selectize: https://shiny.posit.co/r/articles/build/selectize/ 

library(shiny)
library(ggplot2)
library(tidyverse)
library(urbnmapr)
library(usmap)
library(here)

# set up wd
setwd("~/lacmbirds/lacm_birds")
here::i_am("app2.R")

# load data
data5 <- read.csv("data.csv")
specnat <- read.csv(here("specnat.csv"))
sta <- read.csv(here("states.csv"))

## App testing grounds ## 

# Define UI for application that draws a histogram
autocomplist <- data5$species

ui <- fluidPage(
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel(
        titlePanel("Species summary"),
        textInput("sp", "Species"),
        fluidRow(column(12, tableOutput("specnat"))),
        
        fluidRow(column(12, plotOutput("trend"))),
        
        fluidRow(column(12, plotOutput("trend2"))),
        
        
        fluidRow(column(12, plotOutput("state"))),
        
        fluidRow(column(12, plotOutput("ca_cty"))),
        
        fluidRow(
          column(2, tableOutput("countbyyear")),
          column(3, tableOutput("summary")))
      ),
      
      
      tabPanel(
        titlePanel("LACM"),
        textInput("catalog", "LACM"),
        fluidRow(
          column(6,
                 p("Map only works for USA")
          ) ),
        fluidRow(column(12, tableOutput("catcount")),
                 
                 fluidRow(column(12, plotOutput("specmap"))))
      ),
      
      tabPanel(
        titlePanel("Specimen type"),
        textInput("spc", "Species"),
        fluidRow(column(3,  radioButtons("spectype", h3("Specimen type:"),
                                         choices = c("Study skins" = "ss", "Skeleton" = "sk",
                                                     "All" = "all"), selected = "ss")),
                 column(12, tableOutput("speclist")))
      )
    )
  )
)

# "sp", "Species"
ui <-  fluidPage(
  selectizeInput(
    inputId = 'sp',
    label = 'Species',
    choices = NULL,
    selected = NULL,
    multiple = FALSE, # allow for multiple inputs
    options = NULL), # if TRUE, allows newly created inputs
    
    fluidRow(column(12, tableOutput("specnat"))),
    
    fluidRow(column(12, plotOutput("trend"))),
    
    fluidRow(column(12, plotOutput("trend2")))
    )



### 
server <- function(input, output, session) {
  updateSelectizeInput(session, "sp", choices = autocomplist, selected=character(0), server = TRUE)

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
      theme(text=element_text(family="sans")) +
      labs(fill = "Specimen type", color = "Specimen type", x = "Year", y = "Count", title = "Specimen count by year")
    
  }, res = 96)
  
  # count by month and type
  output$trend2 <- renderPlot({
    trend1() %>% 
      ggplot(aes(x = month, fill = specnat, color = specnat)) +
      geom_bar(position = position_dodge(preserve = "single")) +
      scale_x_continuous(breaks = seq(1, 12, 1), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
      theme_classic() +
      theme(text=element_text(family="sans")) +
      labs(fill = "Specimen type", color = "Specimen type", x = "Month", y = "Count", title = "Specimen count by month")
    
  }, res = 96)
}

# Run the application 
shinyApp(ui = ui, server = server)

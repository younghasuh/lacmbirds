library(plotly)
library(shiny)
library(leaflet)
library(ggiraph)
library(htmltools)
library(shinydashboard)
library(tidyverse)
library(DT)


shinyUI(
  
  fluidPage(
    titlePanel(title =  div(img(src="NHM_logo_black_250.png", width="50px", height="50px"), 
                            "LACM Collection Information"), windowTitle = "LACM Collection Information"),
    
    tabsetPanel(
      type = "tabs",
      
      # tab 1 ---
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
                        tabPanel(title = "Summary", 
                                 fluidRow(column(12, h4("Specimen count by specimen type/nature"), tableOutput("specnat"))),
                                 fluidRow(column(3, tableOutput("sexcount")),
                                          column(3, tableOutput("agecount")))),
                        
                        tabPanel(title = "Count data", 
                                 fluidRow(column(12, h4("Specimen count by year"), plotOutput("trend"))),
                                 fluidRow(column(12, h4("Specimen count by month"), plotOutput("trend2"))),
                                 fluidRow(column(12, h4("Specimen count by state (US only)"), plotOutput("state"))),
                                 fluidRow(column(12, h4("Specimen count by county (CA only)"), plotOutput("ca_cty"))),
                                 fluidRow(column(12, h4("Global specimen distribution"), leafletOutput(outputId = 'map'))),
                                 fluidRow(column(12, h4("List of all specimens"), tableOutput("summary")))
                        ),
                        
                        tabPanel(title = "Weights by sex",
                                 fluidRow(column(8, h4("Weights by sex - interactive"), girafeOutput("wtPlot2")),
                                          column(4, h4("Hovering points"), verbatimTextOutput("console"),
                                                 h4("Selected points"), tableOutput("datatab")))
                        ),
                        
                        tabPanel(title = "Data explorationg with Plotly",
                                 fluidRow(column(12, h4("Plotly boxplots"), 
                                                 box(selectInput("xaxis", "Select variable (x-axis)",
                                                                 choices = c("sex", "spp", "state", "month"))))),
                                 fluidRow(column(12, plotlyOutput("plot")))
                        ),
                        
                        tabPanel(title = "Table of all specimens",
                                 fluidRow(column(12, h4("List of all specimens. Use shift or ctrl to select multiple rows for copying onto clipboard."),
                                                 DTOutput("spectab"))))
            )
          ) # mainpanel end
        ) # sidebar layout end
      ), # tabPanel end
      
      # tab 2 ----
      tabPanel(
        titlePanel("LACM lookup"),
        textInput("catalog", "LACM"),
        fluidRow(column(12, tableOutput("catcount"))),
        fluidRow(column(12, h4("Leaftlet map"), leafletOutput(outputId = 'catmap')))
      ),
      
      
      # tab 3 ----
      tabPanel(
        titlePanel("Explore the collection"),
        fluidRow(h4("Last updated 24 Oct 2023")),
        fluidRow(column(12, h4("Total number of specimens"), tableOutput("lacmsumm"))),
        fluidRow(column(12, selectInput("category", "Select category:",
                                        choices = c("Description", "Sex", "family", "genus", "year", "country")))),
        fluidRow(column(12, DTOutput("toptable")))
      )
    )
  )
)
# ui.R

library(shiny)
library(leaflet)


shinyUI(fluidPage(
  tags$head(
    tags$style(HTML('* {font-family: "Arial"};'))),
  
  titlePanel("LACM Specimen trends and maps"),
  
  mainPanel(
    tabsetPanel(
      type = "tabs",
      
      # tab 1
      tabPanel(
        titlePanel("Species summary"),
        
        sidebarLayout(
          sidebarPanel(
            
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
    )
  )))
)
      
      # tab 3
      #tabPanel(
      #  titlePanel("Specimen type (not finished)"),
      #  textInput("spc", "Species"),
      #  fluidRow(column(3,  radioButtons("spectype", h3("Specimen type:"),
      #                                   choices = list("Study skins" = "ss", "Skeleton" = "sk",
      #                                                  "All" = "all"), selected = "ss")),
      #           column(12, tableOutput("speclist")))
library(shiny)
library(leaflet)
library(ggiraph)


shinyUI(fluidPage(
  
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
        
        #fluidRow(column(12, h4("Weights by sex - interactive"), girafeOutput("wtPlot2"))),
        
         fluidRow(column(8, h4("Weights by sex - interactive"), girafeOutput("wtPlot2")),
                  column(4, h4("Hovering points"), verbatimTextOutput("console"),
                         h4("Selected points"), tableOutput("datatab"))),
        
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
# ui.R

library(shiny)


shinyUI(fluidPage(
  tags$head(
    tags$style(HTML('* {font-family: "Arial"};'))),
  
  titlePanel("LACM Specimen trends and map"),
  
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel(
        titlePanel("Species summary"),
        
        selectizeInput(
          inputId = 'sp',
          label = 'Species',
          choices = NULL,
          selected = NULL,
          multiple = FALSE, # allow for multiple inputs
          options = NULL),
        
        fluidRow(column(12, h4("Specimen count by specimen type/nature"), tableOutput("specnat"))),
        
        fluidRow(column(12, h4("Specimen count by year"), plotOutput("trend"))),
        
        fluidRow(column(12, h4("Specimen count by month"), plotOutput("trend2"))),
        
        fluidRow(column(12, h4("Specimen count by state (US only)"), plotOutput("state"))),
        
        fluidRow(column(12, h4("Specimen count by county (CA only)"), plotOutput("ca_cty"))),

        fluidRow( 
          column(2, tableOutput("countbyyear")),
          column(3, tableOutput("summary")))
      ),
      
      # tab 2
      tabPanel(
        titlePanel("LACM"),
        textInput("catalog", "LACM"),
        fluidRow(column(12, tableOutput("catcount")),
        fluidRow(column(12, plotOutput("specmap"))))
      )
    )
  )))
      
      # tab 3
      #tabPanel(
      #  titlePanel("Specimen type (not finished)"),
      #  textInput("spc", "Species"),
      #  fluidRow(column(3,  radioButtons("spectype", h3("Specimen type:"),
      #                                   choices = list("Study skins" = "ss", "Skeleton" = "sk",
      #                                                  "All" = "all"), selected = "ss")),
      #           column(12, tableOutput("speclist")))
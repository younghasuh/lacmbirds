# ui.R

library(shiny)


shinyUI(fluidPage(
  
  titlePanel("LACM Specimen trends and map"),
  
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel(
        titlePanel("Species summary"),
        textInput("sp", "Species"),
        fluidRow(column(12, tableOutput("specnat"))),
        
        fluidRow(column(12, plotOutput("trend"))),
        
        fluidRow(column(12, plotOutput("state"))),
        
        fluidRow(column(12, plotOutput("ca_cty"))),
        
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
      ),
      
      # tab 3
      tabPanel(
        titlePanel("Specimen type (not finished)"),
        textInput("spc", "Species"),
        fluidRow(column(3,  radioButtons("spectype", h3("Specimen type:"),
                                         choices = list("Study skins" = "ss", "Skeleton" = "sk",
                                                        "All" = "all"), selected = "ss")),
                 column(12, tableOutput("speclist")))
      )
    )
  ))
)
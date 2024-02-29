library(shiny)
library(ggplot2)
library(tidyverse)
library(here)
library(plotly)
library(ggiraph)

# set up wd
setwd("~/lacmbirds/lacm_birds")
here("app4.R")

# load data
data <- read.csv("data_2023.csv")

toplist <- data %>% 
  group_by(species) %>%
  summarise(n=n()) 
  slice_max(n, n=10)

# set up autofill species list
alist <- sort(unique(unlist(data$species, use.names = FALSE)))

# set up independent variable options
ind <- c("sex", "spp", "state", "month")


# set up UI
ui <- shinyUI( mainPanel(
  tabsetPanel(
    type = "tabs",
    
    tabPanel(
      titlePanel("Species summary"),
      
      sidebarLayout(
        sidebarPanel(
          h2("Species"),
          p("Start typing species and select from drop-down list."),
          
          # select species of interest
          selectizeInput(inputId = 'sp', label = 'Species', 
                         choices = NULL, selected = NULL, multiple = FALSE, options = NULL),
          
          selectInput("subsp", "Subspecies", choices = NULL)
        ),
          
        
        mainPanel(
#          fluidRow(column(12, h4("List"), tableOutput("listtab"))),
          
          fluidRow(column(12, h4("Summary"), tableOutput("summtab"))),
          
          fluidRow(column(12, h4("Plotly boxplots"), 
                          box(selectInput("xaxis", "select x-axis",
                                          choices = ind)),
                          box(plotlyOutput("plot")))),
          
#          fluidRow(column(12, h4("Plotly boxplots"), plotlyOutput("boxpl"))),
          
#          fluidRow(column(12, h4("Plotly boxplots"), plotlyOutput("boxpl_st"))),
          
#          fluidRow(column(12, h4("Weight boxplot"), girafeOutput("plot")))
          
        )
      ))
    ))
  )




server <- function(input, output, session) {
  
  # 1. Provide species list
  updateSelectizeInput(session, "sp", choices = alist, selected=character(0), server = TRUE)
  
  # 2. select species
  selected <- reactive(data %>% filter(species == input$sp))
  
  # 3. select subspecies within species selection
  observeEvent(selected(), {
    choices <- unique(selected()$spp)
    updateSelectInput(inputId = "subsp", choices = choices) 
  })
  # need to be able to skip if no subspecies present
  
  # 4. reactive for subspecies
  subspecies <- reactive({
    req(input$subsp)
    filter(selected(), spp == input$subsp)
  })
  
  # 5. list table for subspecies
  output$listtab <- renderTable({
    subspecies() %>% 
      select(lacm, sex, wt)
  })
  
  # 6. summary table
  output$summtab <- renderTable({
    subspecies() %>% 
      group_by(sex) %>%
      filter(!is.na(wt)) %>%  # exclude observations without values
      summarise(mean = mean(wt), 
                std = sd(wt),
                n = n())
  })
  
  # plotly boxplots - by subspecies
  # output$boxpl <- renderPlotly({
  #   dat <- selected()
  #   plot_ly(dat, x = dat$spp, y = dat$wt, color = dat$sex, type = "box")  %>% 
  #       layout(boxmode = "group", 
  #              xaxis = list(title='Subspecies'), 
  #              yaxis = list(title='Weight (g)'))
  # })
  
  # plotly boxplots - by state
  # output$boxpl_st <- renderPlotly({
  #   dat <- selected()
  #   plot_ly(dat, x = dat$state, y = dat$wt, color = dat$sex, type = "box")  %>% 
  #     layout(boxmode = "group", 
  #            xaxis = list(title='State'), 
  #            yaxis = list(title='Weight (g)'))
  # })
  
  # plotly boxplots - variable y
  x <- reactive({
    x <- selected()[[input$xaxis]]
  })
  
  output$plot <- renderPlotly({
    dat <- selected()
    plot_ly(dat, x = x(), y = dat$wt, type = "box")  %>% 
      layout(boxmode = "group", 
             xaxis = list(title='Grouping'), 
             yaxis = list(title='Weight (g)'))
  })
  
  ## 
  # For ggiraph plots
  
#  output$plot <- renderGirafe({
    
#    gg_bx <- ggplot(selected(), aes(x=sex, y=wt)) +
#      stat_boxplot(geom="errorbar", position="dodge2") +
#      geom_boxplot(stat="boxplot", position="dodge2", outlier.shape = NA) +
#      geom_point_interactive(aes(tooltip=lacm, data_id=lacm),
#                             size=3, hover_nearest=T, position=position_jitter(0.2))  +
#      scale_x_discrete(limits = c("M", "F", "U"), labels = c("Male", "Female", "Unknown")) +
#      labs(x = "Sex", y = "Weight (g)")
#    girafe(ggobj = gg_bx)
#  })
  
}  

# Run the application 
shinyApp(ui = ui, server = server)

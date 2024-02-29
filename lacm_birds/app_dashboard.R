library(shiny)
library(ggplot2)
library(tidyverse)
library(here)
library(plotly)
library(ggiraph)
library(bs4Dash)
library(DT)

# https://bookdown.org/paul/shiny_workshop/05-visualization.html


# using dashboard
ui <- dashboardPage( 
  
  title = "Species statistics",
  
  ## 1. Header ----
  header = dashboardHeader(
    title = tagList(img(src = "../lacm_birds/NHM_logo_black_250.png", width = 35, height = 35),
                    span("NHMLAC Dashboard", class = "brand-text")
    )
  ),
  
  ## 2. Sidebar ----
  
  sidebar = dashboardSidebar(
    id = "sidebar",
    sidebarMenu(
      id = "sidebarMenu",
      menuItem(tabName = "tab_intro", text = "Introduction", icon = icon("home")),
      menuItem(tabName = "tab_tabulate", text = "Tabulate data", icon = icon("table")),
      flat = TRUE
    ),
    minified = TRUE,
    collapsed = TRUE,
    fixed = FALSE,
    skin = "light"
  ),
  
  ## 3. Body ----
  body = dashboardBody(
    tabItems(
      ### 3.1.1 Tab: Introduction ----
      tabItem(
        tabName = "tab_intro",
        jumbotron(
          title = "The LACM Ornithology Dashboard",
          lead = "A Shiny app to explore LACM Ornithology dataset.",
          status = "info",
          btnName = NULL
        ),
        fluidRow(
          column(width = 1),
          column(
            width = 6,
            box(
              title = "About",
              status = "primary",
              width = 12,
              blockQuote(HTML("This is a test"),
                         color = "primary"),
              p(HTML("This is a test 2")),
              hr(),
              accordion(
                id = "accord",
                accordionItem(
                  title = "References",
                  status = "primary",
                  solidHeader = FALSE,
                  "The following sources are referenced in this app:",
                  tags$ul(
                    class = "list-style: none",
                    style = "margin-left: -30px;",
                    p("testing")
                  )
                ),
                accordionItem(
                  title = "Details",
                  status = "primary",
                  solidHeader = FALSE,
                  p("Last update: Feb 2024"))
              )
            )
          ),
          column(
            width = 4,
            box(
              title = "Young Ha Suh",
              status = "primary",
              width = 12
            )
          )
        )
      ),
      ### 3.3.2 Tab: Tabulate data ----
      tabItem(
        tabName = "tab_tabulate",
        fluidRow(column(12, h4("List"), tableOutput("listtab"))
                 
        )
      ) # end tabItems
    ),
    
    ## 3.4 Footer (bottom)----
    footer = dashboardFooter(
      left = span(
        "This dashboard was created by Jonas Lieth and Paul Bauer. Find the source code",
        a("here.", href = "https://github.com/paulcbauer/shiny_workshop/tree/main/shinyapps/guerry"),
        "It is based on data from the",
        a("Guerry R package.", href = "https://cran.r-project.org/web/packages/Guerry/index.html")
      )
    ),
    ## 3.5 Controlbar (top)----
    controlbar = dashboardControlbar(
      div(class = "p-3", skinSelector()),
      skin = "light"
    )  
  ))


server <- function(input, output, session) {
  
  
  ## 4.1 Tabulate data ----
  ### Variable selection ----
  tab <- reactive({
    var <- input$tab_tabulate_select
    data_table <- data %>% filter(species == "Bubo virginianus")
    
    if (!is.null(var)) {
      data_table <- data_table[, var]
    }
    
    data_table
  })
  
  ### Create table----
  dt <- reactive({
    tab <- tab()
    ridx <- ifelse("Department" %in% names(tab), 3, 1)
    DT::datatable(
      tab,
      class = "hover",
      extensions = c("Buttons"),
      selection = "none",
      filter = list(position = "top", clear = FALSE),
      style = "bootstrap4",
      rownames = FALSE,
      options = list(
        dom = "Brtip",
        deferRender = TRUE,
        scroller = TRUE,
        buttons = list(
          list(extend = "copy", text = "Copy to clipboard"),
          list(extend = "pdf", text = "Save as PDF"),
          list(extend = "csv", text = "Save as CSV"),
          list(extend = "excel", text = "Save as JSON", action = DT::JS("
          function (e, dt, button, config) {
            var data = dt.buttons.exportData();
  
            $.fn.dataTable.fileSave(
              new Blob([JSON.stringify(data)]),
              'Shiny dashboard.json'
            );
          }
        "))
        )
      )
    )
  })
}  
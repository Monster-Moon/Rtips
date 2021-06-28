rm(list = ls())
gc(reset = T)

if(!require(shiny)) install.packages('shiny'); library(shiny)
if(!require(shinydashboard)) install.packages('shinydashboard'); library(shinydashboard)
if(!require(DT)) install.packages('DT'); library(DT)
if(!require(plotly)) install.packages('plotly'); require(plotly)

load('tmp_shiny.Rdata')

#### header ####
header = dashboardHeader(title = 'Axie dashboard')

#### sidebar ####
sidebar = dashboardSidebar(
  sidebarMenu(
    id="tabs",
    menuItem("Options", icon = icon("th"), tabName = "options", badgeColor = "green"),
    menuItem("Charts", icon = icon("line-chart"), tabName = "charts", badgeColor = "green"),
    menuItem("Part3", tabName = "part3", icon=icon("mortar-board"), selected = T)
  )
)

#### body ####
body = dashboardBody(
  tabItems(
    tabItem(tabName = "options",
            h4("Choose options to check"),
            fluidRow(
              column(width = 3, 
                     dateRangeInput(
                       inputId = "dates",
                       label = h3("Date range"),
                       start = min(as.Date(tmp$block_timestamp)),
                       end = max(as.Date(tmp$block_timestamp)),
                       min = min(as.Date(tmp$block_timestamp)),
                       max = max(as.Date(tmp$block_timestamp))),
                    selectInput('class', 'class', choices = c('all', unique(tmp$class)), selected = 'all'),
                    selectInput('mouth_class', 'mouth_class', choices = NULL),
                    selectInput('horn_class', 'horn_class', choices = NULL),
                    selectInput('back_class', 'back_class', choices = NULL),
                    selectInput('tail_class', 'tail_class', choices = NULL),
                    selectInput('eyes_class', 'eyes_class', choices = NULL),
                    selectInput('ears_class', 'ears_class', choices = NULL)
                    ),
              column(3, 
                     tableOutput('data')
                     )
              )
            ),
    tabItem(tabName = "part3",
            fluidPage(
              includeHTML("part3.html")
            )),
    tabItem(tabName = "charts",
            h4("charts"),
            fluidPage(
              column(width = 4,
                     plotOutput('graph1')
                     )
            )
            
    )
  )
)


ui = dashboardPage(
  header,
  sidebar,
  body
)


server = function(input, output) 
{
  class = reactive({
    tmp %>% filter(class == input$class | 'all' == input$class) %>%
      filter()
  })
  
  # substr(tmp$block_timestamp[1:2], 1, 10)
  # input$date = c('')
  # 
  # date_inp = reactive({
  #   input$date
  # })
  # 
  observeEvent(class(), {
    mouth_unique = unique(class()$mouth_class)
    horn_unique = unique(class()$horn_class)
    back_unique = unique(class()$back_class)
    tail_unique = unique(class()$tail_class)
    eyes_unique = unique(class()$eyes_class)
    ears_unique = unique(class()$ears_class)
    updateSelectInput(inputId = 'mouth_class', choices = c('all', mouth_unique), selected = 'all')
    updateSelectInput(inputId = 'horn_class', choices = c('all', horn_unique), selected = 'all')
    updateSelectInput(inputId = 'back_class', choices = c('all', back_unique), selected = 'all')
    updateSelectInput(inputId = 'tail_class', choices = c('all', tail_unique), selected = 'all')
    updateSelectInput(inputId = 'eyes_class', choices = c('all', eyes_unique), selected = 'all')
    updateSelectInput(inputId = 'ears_class', choices = c('all', ears_unique), selected = 'all')
  })

  react_df = reactive({
    class() %>%
      filter(mouth_class == input$mouth_class | input$mouth_class == 'all')  %>%
      filter(horn_class == input$horn_class   | input$horn_class == 'all') %>%
      filter(back_class == input$back_class   | input$back_class == 'all') %>%
      filter(tail_class == input$tail_class   | input$tail_class == 'all') %>%
      filter(eyes_class == input$eyes_class   | input$eyes_class == 'all') %>%
      filter(ears_class == input$ears_class   | input$ears_class == 'all') %>%
      select(block_timestamp, value, exp, skill, morale, speed, hp) %>% 
      arrange(block_timestamp)
  })
  
  output$data = renderTable({
    react_df() %>% mutate(block_timestamp = substr(block_timestamp, 1, 10))
  })
  
  output$graph1 = renderPlot({
    plot(value ~ as.Date(block_timestamp), react_df(), type = 'l', xlab = 'Timestamp', ylab =  'Value')
  })
  
}

shinyApp(ui, server)

rm(list = ls())
gc(reset = T)

if(!require(shiny)) install.packages('shiny'); library(shiny)
if(!require(shinydashboard)) install.packages('shinydashboard'); library(shinydashboard)
if(!require(dplyr)) install.packages('dplyr'); library(dplyr)


load('tmp_shiny.Rdata')
tmp = tmp %>% mutate(block_timestamp = as.Date(block_timestamp))

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
                       inputId = "date",
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
              column(4, 
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
            plotOutput('hist1'),
            plotOutput('hist2'),
            plotOutput('hist3'),
            plotOutput('hist4'),
            plotOutput('hist5'),
            plotOutput('graph1'))
          
            # fluidPage(
            #   fluidRow(
            #     column(3,
            #            plotOutput('hist1')
            #            ),
            #     column(width = 3,
            #            plotOutput('hist2')
            #            ),
            #     column(width = 3,
            #            plotOutput('hist3')
            #            )),
            #   fluidRow(
            #     column(3,
            #            plotOutput('hist4')
            #            ),
            #     column(width = 3,
            #            plotOutput('hist5')
            #            ),
            #     column(width = 3,
            #            plotOutput('hist6')
            #            )),
            #   fluidRow(
            #     column(3,
            #            plotOutput('hist4')
            #            ),
            #     column(width = 3,
            #            plotOutput('hist5')
            #            ))
    )
  )
    



#### server ####
server = function(input, output) 
{
  init_df = reactive({
    tmp %>% filter(class == input$class | 'all' == input$class) %>%
      filter(between(block_timestamp, as.Date(input$date[1]), as.Date(input$date[2])))
  })
  
  observeEvent(init_df(), {
    mouth_unique = unique(init_df()$mouth_class)
    horn_unique = unique(init_df()$horn_class)
    back_unique = unique(init_df()$back_class)
    tail_unique = unique(init_df()$tail_class)
    eyes_unique = unique(init_df()$eyes_class)
    ears_unique = unique(init_df()$ears_class)
    
    updateSelectInput(inputId = 'mouth_class', choices = c('all', mouth_unique), selected = 'all')
    updateSelectInput(inputId = 'horn_class', choices = c('all', horn_unique), selected = 'all')
    updateSelectInput(inputId = 'back_class', choices = c('all', back_unique), selected = 'all')
    updateSelectInput(inputId = 'tail_class', choices = c('all', tail_unique), selected = 'all')
    updateSelectInput(inputId = 'eyes_class', choices = c('all', eyes_unique), selected = 'all')
    updateSelectInput(inputId = 'ears_class', choices = c('all', ears_unique), selected = 'all')
  })

  react_df = reactive({
    init_df() %>%
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
  
  output$hist1 = renderPlot({
    hist(react_df()$exp)
  })
  
  output$hist2 = renderPlot({
    hist(react_df()$skill)
  })
  
  output$hist3 = renderPlot({
    hist(react_df()$morale)
  })
  
  output$hist4 = renderPlot({
    hist(react_df()$speed)
  })
  
  output$hist5 = renderPlot({
    hist(react_df()$hp)
  })

}


ui = dashboardPage(
  header,
  sidebar,
  body
)

shinyApp(ui, server)


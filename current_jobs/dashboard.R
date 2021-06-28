rm(list = ls())
gc(reset = T)


if(!require(shiny)) install.packages('shiny'); library(shiny)
if(!require(shinydashboard)) install.packages('shinydashboard'); library(shinydashboard)


header = dashboardHeader(title = 'Test')

sidebar = dashboardSidebar(
  sidebarMenu(
    id="tabs",
    menuItem("Options", icon = icon("th"), tabName = "options", badgeColor = "green"),
    menuItem("Charts", icon = icon("line-chart"), tabName = "charts", badgeColor = "green",
             menuSubItem("trend", tabName = "trend", icon = icon("angle-right")),
             menuSubItem("Prediction", tabName = "prediction", icon = icon("angle-right"))),
    menuItem("ReadMe", tabName = "readme", icon=icon("mortar-board"), selected = T)
  )
)

body = dashboardBody(
  tabItems(
    tabItem(tabName = "options",
            h4("Choose options to check"),
            sliderInput(inputId = 'test', label = 'test', min = 1, max = 3, value = 2)),
    tabItem(tabName = "readme",
            fluidPage(
              tags$iframe(src = 'tmp.html', 
                          width = '100%', height = '800px', 
                          frameborder = 0, scrolling = 'auto')
            )),
    tabItem(tabName = "trend",
            h4("trend"),
            plotOutput('plot1')),
    tabItem(tabName = "prediction",
            h4("prediction"))
  )
)

ui = dashboardPage(
  header,
  sidebar,
  body
)

server = function(input, output) 
{
  test_r = reactive({
    input$test
  })
  
  output$plot1 = renderPlot({
    plot(test_r()) 
  })
}
shinyApp(ui, server)
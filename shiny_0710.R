

rm(list = ls())
gc(reset = T)

if(!require(shiny)) install.packages('shiny'); library(shiny)
if(!require(shinydashboard)) install.packages('shinydashboard'); library(shinydashboard)
if(!require(dplyr)) install.packages('dplyr'); library(dplyr)
if(!require(plotly)) install.packages('plotly'); library(plotly)
if(!require(leaflet)) install.packages('leaflet'); library(leaflet)

#### header ####
header = dashboardHeader(title = 'Test dashboard')

#### sidebar ####
sidebar = dashboardSidebar(
  sidebarMenu(
    id="tabs",
    menuItem("Part1", icon = icon("line-chart"), tabName = "part1", badgeColor = "green", selected = T),
    menuItem("Part2", icon = icon("map-marked-alt"), tabName = "part2", badgeColor = "green")
  )
)

#### body ####
body = dashboardBody(
  tabItems(
    tabItem(tabName = "part1",
            sidebarLayout(
              sidebarPanel(
                width = 3,
                dateRangeInput(
                  inputId = "date",
                  label = "Date range",
                  start = as.Date(Sys.time()) - 100,
                  end = as.Date(Sys.time()),
                  min = as.Date(Sys.time()) - 100,
                  max = as.Date(Sys.time())),
                selectInput(
                  inputId = 'species',
                  label = 'Species',
                  choices = c('all', as.character(unique(iris$Species)))
                ),
                uiOutput('slider_ui')
                ),
              mainPanel(
                fluidRow(
                  column(width = 5,
                         plotlyOutput('plot1'),
                         plotlyOutput('plot3'),
                         # textOutput('text'),
                         plotlyOutput('plot5')
                         ),
                  column(width = 5,
                         plotlyOutput('plot2'),
                         plotlyOutput('plot4'),
                         plotlyOutput('plot6')
                         )
                )
                )
              )
            ),
    tabItem(tabName = "part2",
            # h4("Map"),
            leafletOutput('map', width = '100%', height = 1024)
            )
  )
)

#### server ####
server = function(input, output) 
{
  iris_reactive = reactive({
    if(input$species == 'all')
    {
      iris
    }else
    {
      iris %>% dplyr::filter(Species == input$species)
    }
  })
  
  output$slider_ui = renderUI({
    tmp_iris = iris_reactive() %>% select(-Species)
    output_list = tagList()
    for(i in 1:ncol(tmp_iris))
    {
      val = iris_reactive()[, i]
      output_list[[i]] = sliderInput(colnames(iris_reactive()[i]), 
                                     colnames(iris_reactive()[i]), 
                                     min = min(val), 
                                     max = max(val), 
                                     value = quantile(val, c(0.05, 0.95)))
    }
    output_list
  })
  
  iris_vis_reactive = reactive({
    iris_reactive() %>%
      dplyr::filter(between(Sepal.Length, input$Sepal.Length[1], input$Sepal.Length[2]),
                    between(Sepal.Width, input$Sepal.Width[1], input$Sepal.Width[2]),
                    between(Petal.Length, input$Petal.Length[1], input$Petal.Length[2]),
                    between(Petal.Width, input$Petal.Width[1], input$Petal.Width[2]))
  })
  
  output$plot1 = renderPlotly({
    plot_ly(iris_vis_reactive(), x = ~Sepal.Length, y = ~Sepal.Width) %>% add_markers()
  })

  output$plot2 = renderPlotly({
    plot_ly(iris_vis_reactive(), x = ~Petal.Length, y = ~Petal.Width) %>% add_markers()
  })
  output$plot3 = renderPlotly({
    plot_ly(iris_vis_reactive(), x = ~Sepal.Length) %>% add_histogram()
  })
  output$plot4 = renderPlotly({
    plot_ly(iris_vis_reactive(), x = ~Sepal.Width) %>% add_histogram()
  })
  output$plot5 = renderPlotly({
    plot_ly(iris_vis_reactive(), x = ~Petal.Length) %>% add_histogram()
  })
  output$plot6 = renderPlotly({
    plot_ly(iris_vis_reactive(), x = ~Petal.Width) %>% add_histogram()
  })
  
  output$map = renderLeaflet({
    lat1 = c(37.584750, 37.584548, 37.584584, 37.584433, 37.584449, 37.584821)
    lon1 = c(127.055991, 127.056084, 127.056795, 127.056808, 127.057181, 127.057146)
    
    lat2 = c(37.584037, 37.583815, 37.583832, 37.584053)
    lon2 = c(127.055479, 127.055484, 127.055908, 127.055876)
    
    df = rbind(data.frame(group = 1, lat = lat1, lon = lon1, mag = 1), 
               data.frame(group = 2, lat = lat2, lon = lon2, mag = 10))
    leaflet(df) %>% addTiles() %>% 
      setView(lng = mean(df$lon), lat = mean(df$lat), zoom = 15) %>%
      # fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat)) %>% 
      addCircleMarkers()
  })
}


ui = dashboardPage(
  header,
  sidebar,
  body
)

shinyApp(ui, server)


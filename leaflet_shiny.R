if(!require(leaflet)) install.packages('leaflet'); require(leaflet)
if(!require(shiny)) install.packages('shiny'); require(shiny)

library(RColorBrewer)

lat1 = c(37.584750, 37.584548, 37.584584, 37.584433, 37.584449, 37.584821)
lon1 = c(127.055991, 127.056084, 127.056795, 127.056808, 127.057181, 127.057146)

lat2 = c(37.584037, 37.583815, 37.583832, 37.584053)
lon2 = c(127.055479, 127.055484, 127.055908, 127.055876)

df = rbind(data.frame(group = 1, lat = lat1, lon = lon1, mag = 1), 
           data.frame(group = 2, lat = lat2, lon = lon2, mag = 10))


ui = bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                selectInput('group1', 'group', choices = unique(df$group))
                )
)


server = function(input, output, session){
  output$map = renderLeaflet({
    leaflet(df) %>% addTiles() %>% 
      fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
  })
  
  filtered_data = reactive({
    df %>% dplyr::filter(group == input$group1)
  })
  
  colorpal <- reactive({
    colorNumeric('BrBG', df$mag)
  })
  
  observe({
    pal = colorpal()
    proxy = leafletProxy('map', data = filtered_data()) %>% 
      clearShapes()
    proxy %>% addPolygons(lng = filtered_data()$lon, 
                          lat = filtered_data()$lat, 
                          fillColor = ~pal(mag),
                          fillOpacity = 0.7)
  })
}


quakes
shinyApp(ui, server)

myServer <- function(input, output) {
  output$worldmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = "CartoDB") %>%
      addMarkers(lng = coronavirus$Long, lat = coronavirus$Lat)
  })
  
  output$full_data = renderDataTable({coronavirus})
}
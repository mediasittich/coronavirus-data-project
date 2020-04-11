myServer <- function(input, output) {
  
  # World Map
  output$worldmap <- renderLeaflet({
    leaflet() %>%
      # Basemap
      addProviderTiles(provider = "CartoDB") #%>%
      # Add layer for countries with input date from user (slider)
      # choropleth with pop-ups
  })
  
  output$full_data = renderDataTable({coronavirus})
}
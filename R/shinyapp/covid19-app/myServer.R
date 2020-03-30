myServer <- function(input, output) {
  output$worldmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = "CartoDB") #%>%
      # Add layer for countries with input date from user (slider)
  })
  
  output$full_data = renderDataTable({coronavirus})
}
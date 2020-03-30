myUI <- navbarPage("Corona Virus (COVID-19) Project",
  
  tabPanel("Map",
    leafletOutput("worldmap")
  ),
  tabPanel("Graphs"),
  tabPanel("Data",
    dataTableOutput("full_data")
  ),
  tabPanel("About",
     tags$div(
       tags$h4("About this project...")
     )
  )
)
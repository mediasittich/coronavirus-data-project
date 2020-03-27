library(shiny)
library(plotly)


# Define UI
ui <- fluidPage(
    
    titlePanel(("Corona Virus (COVID-19) Project")),
    
    # Selector fields for user input
    fluidRow(
        
        # Country/Region selector dropdown
        column(
            width = 4,
            selectizeInput("country", label = h5("Country/Region"), choices = NULL, width = "100%")
        ),
        # State/Province selector dropdown
        column(
            width = 4,
            selectizeInput("state", label = h5("State/Province"), choices = NULL, width = "100%")
        ),
        # Type selector dropdown
        column(
            width = 4,
            checkboxGroupInput(
                "types", label = h5("Case Types"),
                choices = c("Confirmed", "Death", "Recovered"),
                selected = c("Confirmed", "Death", "Recovered"),
                width = "100%"
            )
        )
        
    ),
    
    # Plot to display cumulated cases
    fluidRow(
        plotlyOutput("cumulativeMetrics")
    )
)


# Define server logic
server <- function(input, output) {}


# Run the app
shinyApp(ui = ui, server = server)

############################# DEPLOY TO shinyapps.io #############################

# RUN COMMANDS:
# library(rsconnect)
# rsconnect::deployApp('shinyapp/covid19-app')
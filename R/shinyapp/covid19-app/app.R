library(shiny)
library(dplyr)
library(tidyr)
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
server <- function(input, output, session) {
    
    # Define data source
    base_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"
    
    # Load data and pre-process
    load_data <- function(fileName, columnName) {
        if (!file.exists(fileName)) {
            data <- read.csv(file.path(base_url, fileName), check.names = FALSE, stringsAsFactors = FALSE) %>%
                select(-Lat, -Long) %>%
                pivot_longer(-(1:2), names_to = "date", values_to = columnName) %>%
                mutate(
                    date = as.Date(date, format = "%m/%d/%y"),
                    `Province/State` = if_else(`Province/State` == "", "<all>", `Province/State`)
                )
        } else {
            load(file = fileName)
        }
        return(data)
    }
    
    # Save data to data frame
    allData <- load_data("time_series_covid19_confirmed_global.csv", "CumConfirmed") %>%
        inner_join(load_data("time_series_covid19_deaths_global.csv", "CumDeaths")) %>%
        inner_join(load_data("time_series_covid19_recovered_global.csv", "CumRecovered"))
    
    # 
    data <- reactive({
        d <- allData %>%
            filter(`Country/Region` == input$country)
        if (input$state != "<all>") {
            d <- d %>%
                filter(`Province/State` == input$state)
        } else {
            d <- d %>%
                group_by(date) %>%
                summarise_if(is.numeric, sum, na.rm = TRUE)
        }
        d %>%
            mutate(
                dateStr = format(date, format = "%b %d, %Y"),
                NewConfirmed=CumConfirmed - lag(CumConfirmed, default = 0),
                NewRecovered=CumRecovered - lag(CumRecovered, default = 0),
                NewDeaths=CumDeaths - lag(CumDeaths, default = 0)
            )
    })
    
    #
    observeEvent(input$country, {
        states = allData %>%
            filter(`Country/Region` == input$country) %>% 
            pull(`Province/State`)
        states = c("<all>", sort(unique(states)))
        updateSelectInput(session, "state", choices=states, 
                          selected=states[1])
    })
    
    # 
    countries = sort(unique(allData$`Country/Region`))
    
    updateSelectInput(session, "country", choices=countries, 
                      selected="Italy")
    
}


# Run the app
shinyApp(ui = ui, server = server)

############################# DEPLOY TO shinyapps.io #############################

# RUN COMMANDS:
# library(rsconnect)
# rsconnect::deployApp('shinyapp/covid19-app')
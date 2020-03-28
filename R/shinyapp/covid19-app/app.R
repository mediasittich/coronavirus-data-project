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
    
    # Plot
    renderBarPlot = function(varPrefix, legendPrefix, yaxisTitle) {
        renderPlotly({
            data = data()
            plt = data %>% 
                plot_ly() %>%
                config(displayModeBar=FALSE) %>%
                layout(
                    #barmode='group', 
                    xaxis=list(
                        title="", tickangle=-90, type='category', ticktext=as.list(data$dateStr), 
                        tickvals=as.list(data$date), gridwidth=1), 
                    yaxis=list(
                        title=yaxisTitle
                    ),
                    legend=list(x=0.05, y=0.95, font=list(size=15), bgcolor='rgba(240,240,240,0.5)')#,
                    #font=f1
                )
            for(type in input$types) 
                plt = plt %>%
                add_trace(
                    x= ~date, y=data[[paste0(varPrefix, type)]], type='bar', 
                    name=paste(legendPrefix, type, "Cases"),
                    marker=list(
                        color=switch(type, Deaths='rgb(200,30,30)', Recovered='rgb(30,200,30)', Confirmed='rgb(100,140,240)'),
                        line=list(color='rgb(8,48,107)', width=1.0)
                    )
                )
            plt
        })
    }
    #output$dailyMetrics = renderBarPlot("New", legendPrefix="New", yaxisTitle="New Cases per Day")
    output$cumulativeMetrics = renderBarPlot("Cum", legendPrefix="Cumulated", yaxisTitle="Cumulated Cases")
    
}


# Run the app
shinyApp(ui = ui, server = server)

############################# DEPLOY TO shinyapps.io #############################

# RUN COMMANDS:
# library(rsconnect)
# rsconnect::deployApp('shinyapp/covid19-app')
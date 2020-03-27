library(shiny)


# Define UI
ui <- fluidPage(
    sliderInput(inputId = "num",
                label = "Choose a number",
                value = 25, min = 1, max = 100),
    plotOutput("hist")
)


# Define server logic
server <- function(input, output) {
    output$hist <- renderPlot({ hist(rnorm(input$num)) })
}


# Run the app
shinyApp(ui = ui, server = server)

############################# DEPLOY TO shinyapps.io #############################

# RUN COMMANDS:
# library(rsconnect)
# rsconnect::deployApp('shinyapp/covid19-app')
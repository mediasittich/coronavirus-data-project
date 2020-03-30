library(dplyr)
library(tidyr)
library(leaflet)

base_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"

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

allData <- load_data("time_series_covid19_confirmed_global.csv", "CumConfirmed")

allData <- load_data("time_series_covid19_confirmed_global.csv", "CumConfirmed") %>%
  inner_join(load_data("time_series_covid19_deaths_global.csv", "CumDeaths")) %>%
  inner_join(load_data("time_series_covid19_recovered_global.csv", "CumRecovered"))


# MAPS

m <- leaflet() %>%
  addTiles() #%>%
  #addMarkers(lng=174.768, lat=-36.852, popup = "The birthplace of R")
m





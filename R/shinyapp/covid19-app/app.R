library(shiny)
library(coronavirus)
library(leaflet)
library(DT)
library(rgdal)
library(dplyr)
library(tidyr)
library(readr)

source("myUI.R", local = TRUE)
source("myServer.R")

unzip("./dataset/TM_WORLD_BORDERS_SIMPL-0.3.zip", exdir = "./dataset/world_shape_file")

world_spdf <- readOGR(
  dsn = paste0(getwd(),"/dataset/world_shape_file/") , 
  layer = "TM_WORLD_BORDERS_SIMPL-0.3",
  verbose = FALSE
)

world_spdf@data$POP2005[ which(world_spdf@data$POP2005 == 0)] = NA
world_spdf@data$POP2005 <- as.numeric(as.character(world_spdf@data$POP2005)) / 1000000 %>% round(2)

mypalette <- colorNumeric(palette = "viridis", domain = world_spdf@data$POP2005, na.color = "transparent")
mypalette(c(45,43))

m <- leaflet(world_spdf) %>%
  addTiles() %>%
  setView(lat = 10, lng = 0, zoom = 2) %>%
  addPolygons(fillColor = ~mypalette(POP2005), stroke = FALSE)

m

read.csv("./dataset/")

library(utils)
library(httr)

#download the dataset from the ECDC website to a local temporary file
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))

#read the Dataset sheet into “R”. The dataset will be called "data".
data <- read.csv(tf)

# library(rnaturalearth)
# library(dplyr)
# library(ggplot2)
# 
# worldmap <- ne_countries(returnclass = "sf")
# 
# coronavirus_points <- coronavirus_spatial() %>%
#   filter(date == "2020-03-28") %>%
#   filter(type == "confirmed")
# 
# coronavirus_polys <- coronavirus_spatial(return_shape = "polygon") %>%
#   filter(date == "2020-03-28") %>%
#   filter(type == "confirmed")
# 
# ggplot(worldmap) +
#   geom_sf() +
#   geom_sf(data = coronavirus_polys, aes(fill = log10(cases + 1))) +
#   #geom_sf(data = coronavirus_points) +
#   scale_fill_viridis_c(option = "magma", direction = -1) +
#   theme_void()


shinyApp(
  ui = myUI,
  server = myServer
)
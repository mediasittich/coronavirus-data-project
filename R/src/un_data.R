library(readr)
library(dplyr)
library(tidyr)


setwd("~/Google Drive_mediasittich/LMU-Statistik/praxisprojekt_ws1920/coronavirus-data-project/R")

######################### UN Data - Population #########################

# ------------------------ Load data from CSV ------------------------
# file source 'Population': https://data.un.org/Default.aspx

SYB62_1_201907_Population_Surface_Area_and_Density <- read_csv("data/SYB62_1_201907_Population, Surface Area and Density.csv")


# ------------------------ Transform data ------------------------
un_data_population <- select(SYB62_1_201907_Population_Surface_Area_and_Density, -c(X6, X7)) %>%
  rename(
    country_code = T02,
    region_country_area = "Population, density and surface area",
    year = X3,
    series = X4,
    value = X5
  ) %>%
  filter(year == "2019") %>%
  pivot_wider(names_from = series, values_from = value)

un_data_population <- type_convert(un_data_population)

summary(un_data_population)

######################### UN Data - GDP #########################

# file source 'National accounts': https://data.un.org/Default.aspx
SYB62_230_201904_GDP_and_GDP_Per_Capita <- read_csv("data/SYB62_230_201904_GDP and GDP Per Capita.csv")

# ------------------------ Transform data ------------------------
un_data_gdp <- select(SYB62_230_201904_GDP_and_GDP_Per_Capita, -c(X6, X7)) %>%
  rename(
    country_code = "[T13.]",
    region_country_area = "Gross domestic product and gross domestic product per capita",
    year = X3,
    series = X4,
    value = X5
  ) %>%
  filter(year == "2017") %>%
  pivot_wider(names_from = series, values_from = value)

un_data_gdp <- type_convert(un_data_gdp)

summary(un_data_gdp)
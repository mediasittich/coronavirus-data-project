library(coronavirus)
data("coronavirus")

countries <- unique(coronavirus$Country.Region)

geo_data <- data.frame(country = countries)

library(countrycode)

geo_data$continent <- countrycode(sourcevar = countries, origin = "country.name", destination = "continent")


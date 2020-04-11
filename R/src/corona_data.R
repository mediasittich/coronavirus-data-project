#.................................. Load data from package ..........................................
library(coronavirus)

library(readr)
library(dplyr)
library(tidyr)


setwd("~/Google Drive_mediasittich/LMU-Statistik/praxisprojekt_ws1920/coronavirus-data-project/R")

######################### COVID-19 Data #########################

#.................................. Update data from package ..........................................
update_datasets()
data("coronavirus")

####################################### Inspect dataset ############################################

summary(coronavirus)

# copy dataset to working data frame & convert empty cells to NAs
df <- coronavirus %>% mutate_if(is.character, list(~na_if(., "")))

# convert Province.State, Country.State, type from character to factor
df <- mutate_if(df, is.character, as.factor)

str(df)
summary(df)

missing_col <- sapply(df, function(x) sum(is.na(x) )) / nrow(df)
missing_col[missing_col > 0]

###################################### Summary of dataset ##########################################

# df %>%
#   group_by(Country.Region, type) %>%
#   summarise(total_cases = sum(cases)) %>%
#   arrange(total_cases)

###################################### Inspect data grouped by type ##########################################

raw_data_confirmed <- df %>%
  select(-Lat, -Long) %>%
  group_by(Country.Region) %>%
  filter(type == "confirmed")

raw_data_deaths <- df %>%
  select(-Lat, -Long) %>%
  group_by(Country.Region) %>%
  filter(type == "death")

raw_data_recovered <- df %>%
  select(-Lat, -Long) %>%
  group_by(Country.Region) %>%
  filter(type == "recovered")

###################################### CONFIRMED CASES ##########################################

summary(raw_data_confirmed)

library(ggplot2)
library(scales)

confirmed_cases_worldwide <- raw_data_confirmed %>%
  group_by(date) %>%
  summarise(total_cases = sum(cases)) %>%
  mutate(cumsum = cumsum(total_cases))

germany <- raw_data_confirmed %>%
  filter(Country.Region == "Germany") %>%
  select(Country.Region, Province.State, date, cases, type)

ggplot(germany, aes(date, cases)) +
  geom_point() +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x)
  ) +
  annotation_logticks(sides = "bl") +
  labs(x = "Date", y = "Cumulative number of registered Covid-19 infections")

us <- raw_data_confirmed %>%
  filter(Country.Region == "US") %>%
  select(Country.Region, Province.State, date, cases, type)

ggplot(us, aes(date, cases)) +
  geom_point() +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x)
  ) +
  annotation_logticks(sides = "bl") +
  labs(x = "Date", y = "Cumulative number of registered Covid-19 infections")


######################### COVID-19 Data vs UN Data #########################

# Create list of all countries in dataset
countries <- unique(coronavirus$Country.Region)

# Check for countries missing in UN dataset
countries[!countries %in% un_data_population$region_country_area]

# !TODO: research missing countries and replace
# [1] "Bolivia"                          "Brunei"                           "Burma"                           
# [4] "Congo (Brazzaville)"              "Congo (Kinshasa)"                 "Cote d'Ivoire"                   
# [7] "Diamond Princess"                 "Iran"                             "Korea, South"                    
# [10] "Kosovo"                           "Laos"                             "Moldova"                         
# [13] "MS Zaandam"                       "Russia"                           "Saint Vincent and the Grenadines"
# [16] "Syria"                            "Taiwan*"                          "Tanzania"                        
# [19] "US"                               "Venezuela"                        "Vietnam"                         
# [22] "West Bank and Gaza"  


countries[!countries %in% un_data_gdp$region_country_area]

# [1] "Bolivia"                          "Brunei"                           "Burma"                           
# [4] "Congo (Brazzaville)"              "Congo (Kinshasa)"                 "Cote d'Ivoire"                   
# [7] "Diamond Princess"                 "Holy See"                         "Iran"                            
# [10] "Korea, South"                     "Laos"                             "Moldova"                         
# [13] "MS Zaandam"                       "Russia"                           "Saint Vincent and the Grenadines"
# [16] "Syria"                            "Taiwan*"                          "Tanzania"                        
# [19] "US"                               "Venezuela"                        "Vietnam"                         
# [22] "West Bank and Gaza"     

un_data_population$region_country_area[!un_data_population$region_country_area %in% un_data_gdp$region_country_area]


####################################################################################################
#                              Praxisprojekt - WiSe 2019/20                                        #
#         Analysing Coronavirus: Development, reluts in actions/policies, etc.(?)                  #
#                         Regina Galambos, Lorenz Mihatsch                                         #
####################################################################################################

######################################### Preparation #############################################

# Set working directory
setwd("./")

#....................................... Load packages ..............................................
library(dplyr)
library(tidyr)

#.................................. Load data from package ..........................................
library(coronavirus)
data("coronavirus")

####################################### Inspect dataset ############################################

# copy dataset to working data frame & convert empty cells to NAs
df <- coronavirus %>% mutate_if(is.character, list(~na_if(., "")))

# convert Province.State, Country.State, type from character to factor
df <- mutate_if(df, is.character, as.factor)

str(df)
summary(df)

missing_col <- sapply(df, function(x) sum(is.na(x) )) / nrow(df)
missing_col[missing_col > 0]

###################################### Summary of dataset ##########################################

# Examples from GitHub repo
df %>%
  group_by(Country.Region, type) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(total_cases)


df %>% 
  filter(date == max(date)) %>%
  select(country = Country.Region, type, cases) %>%
  group_by(country, type) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type, values_from = total_cases) %>%
  arrange(-confirmed)









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
library(lubridate)
library(plotly)

#.................................. Load data from package ..........................................
library(coronavirus)
data("coronavirus")

update_datasets()

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


germany <- df %>%
  filter(Country.Region == "Germany" & type == "confirmed") %>%
  select(Country.Region, Province.State, date, cases, type) #%>%
  #summarise(total = sum(cases)) #%>%
  #pivot_wider(names_from = type, values_from = total)

germany <- germany %>%
  mutate(cumsum = cumsum(cases))

library(ggplot2)
library(scales)

ggplot(germany, aes(date, cumsum)) +
  geom_point() +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x)
  ) +
  annotation_logticks(sides = "bl") +
  labs(x = "Date", y = "Cumulative number of registered Covid-19 infections")
  #scale_y_continuous(trans = log_trans(),
  #  breaks = trans_breaks("log", function(x) exp(x)),
  #  labels = trans_format("log", math_format(e^.x))) + 
  #annotation_logticks() 

# UN data
# read.csv("../data/SYB62_1_201907_Polutation, Surface Area and Density.csv")





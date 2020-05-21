####################################################################################################
#                              Praxisprojekt - WiSe 2019/20                                        #
#                           Analyse der COVID-19 Fallzahlen                                        #
#                           Regina Galambos, Lorenz Mihatsch                                       #                             
####################################################################################################

# The Analysis starts at 22.01.2020 and ends at 27.04.2020.

###################################### Preparations ###########################################

#........................................... Set working directory ....................................................

# Set working directory relative to current file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#........................................... Load COVID-19 data from package ....................................................

library(coronavirus)
data("coronavirus")

########################################### Loading required Packages ################################################

# Load Packages
library(dplyr) # Version 0.8.5
library(tidyr) # Version 1.0.2
library(lubridate) # Version 1.7.4
library(ggplot2) # Version 3.3.0
library(viridis) # Version 0.5.1
library(coronavirus) # Version 0.2.0
library(psych) # Version 1.9.12.31
library(zoo) # Version 1.8-7

############################################ Inspect dataset #################################################

# copy dataset to working data frame & convert empty cells to NAs
df_all <- coronavirus %>% mutate_if(is.character, list(~na_if(., "")))

str(df_all)
summary(df_all)

missing_col <- sapply(df_all, function(x) sum(is.na(x) )) / nrow(df_all)
missing_col[missing_col > 0]
rm(missing_col)

########################### Begin Descriptive Analysis #############################################

# For the initial data cleaning procedure, countries are treated differently.
# Reason: Australia and China are only given by thier Provinces/Counties --> treated in df_provinces
# Canada is given by its Counties and additionally as Country without provinces --> treated in df_canada
# Denmark, France, Netherlands and UK are given as Country and as some of their 
# former/current colonies. --> treated in df_colonies

# Data contains three ships: Grand Princess, Diamond Princess and MS Zaandam
# Grand Princess will be excluded from the beginning of the Analysis, Diamond Princess and
# MS Zaandam will be expluded for the plots later.

df_all = df_all[-which(df_all$province == "Grand Princess"),] # Removing Grand Princess

df_provinces = df_all%>%
  na.omit(cols="province")%>%
  filter(country == "China" | country == "Australia")%>% # Provinces of Canada not included
  group_by(country, date, type)%>%
  summarise(cases = sum(cases))%>% # by summarizing all other variables are lost.
  ungroup()%>%
  group_by(country, type)%>%
  mutate(cumulative = cumsum(cases))%>% # cumsum: Variable with cumulative sum.
  ungroup()
  
df_colonies = df_all%>%
  na.omit(cols="province")%>%
  filter(country != "China" & country != "Australia" & country != "Canada")%>%
  select(-country)%>%
  rename("country"="province")%>%
  group_by(country, type)%>%
  mutate(cumulative = cumsum(cases))%>% # cumsum: Variable with cumulative sum.
  ungroup()

df_canada = df_all%>%
  filter(country == "Canada")%>%
  group_by(country, date, type)%>%
  summarise(cases = sum(cases))%>% # by summarizing all other variables are lost.
  ungroup()%>%
  group_by(country, type)%>%
  mutate(cumulative = cumsum(cases))%>% # cumsum: Variable with cumulative sum.
  ungroup()

# all other states, except for the above stated.
df_states = df_all %>%
  filter(country != "Canada")%>%
  filter(is.na(province))%>%
  select(-province)%>%
  group_by(country, type)%>%
  mutate(cumulative = cumsum(cases))%>% # cumsum: Variable with cumulative sum.
  ungroup()

# merging the datasets back together.
df_states = df_states%>%
  bind_rows(df_provinces)%>%
  bind_rows(df_colonies)%>%
  bind_rows(df_canada)%>%
  select(-lat, -long) # not relevant for analysis
rm(df_provinces, df_colonies, df_canada, df_all) # tidy up

# This df_world data.frame contains the cumulative world data
df_world = df_states%>%
  select(country, date, cases, type)%>%
  pivot_wider(names_from = c(country), values_from = cases)%>% #changing country to Variables
  ungroup()%>%
  mutate(cases.world = rowSums(.[3:ncol(.)]))%>% #Sum above all countries
  select(date, type, cases.world)%>%
  group_by(type)%>%
  mutate(cumulative.world = cumsum(cases.world)) # Cumulative sum over time

###-----------------------------------------------------------------------------------------
## Get a first impressino by plotting the cumulative cases.
## Plot: Cummulative cases and types vs date for all countries
plot_cumulative_by_time_confirmed = df_states%>%
  group_by(country, type)%>%
  filter(type == "confirmed")%>%
  filter(date <= "2020-04-27")%>%
    ggplot(aes(date, cumulative, fill = country))+
      geom_line()+
      labs(x = "Date", y="cumulative cases", 
           title = "Confirmed cases", subtitle = "Absolute number of cumulative cases for each country")+
      theme_bw()+
      theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))
plot_cumulative_by_time_confirmed

plot_cumulative_by_time_death = df_states%>%
  group_by(country, type)%>%
  filter(type == "death")%>%
  filter(date <= "2020-04-27")%>%
    ggplot(aes(date, cumulative, fill = country))+
      geom_line()+
      labs(x = "Date", y="cumulative deaths", 
       title = "Confirmed deaths", subtitle = "Absolute number of cumulative deaths for each country")+
      theme_bw()+
      theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))
plot_cumulative_by_time_death

plot_cumulative_by_time_recovered = df_states%>%
  group_by(country, type)%>%
  filter(type == "recovered")%>%
  filter(date <= "2020-04-27")%>%
    ggplot(aes(date, cumulative, fill = country))+
      geom_line()+
      labs(x = "Date", y="cumulative recovered", 
       title = "Confirmed recovered", subtitle = "Absolute number of cumulative recovered for each country")+
      theme_bw()+
      theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))
plot_cumulative_by_time_recovered

##---------------------------------------------------------------------------------------------------------
## Get Demographic information and Country Code
countries <- unique(df_states$country) # Countries from the corona dataframe.
geo_data <- data.frame(country = countries) # as data.frane

library(countrycode) # to get the iso2c/iso3c code
library(raster) # to merge the iso2c to the continent from ccodes()

geo_data$ISO3 <- countrycode(sourcevar = countries, origin = "country.name", destination = "iso3c") # get iso3c
geo_data$ISO2 <- countrycode(sourcevar = countries, origin = "country.name", destination = "iso2c") # get iso3c
geo_data$ISO3[which(geo_data$country == "Kosovo")] = "XKX"
geo_data$ISO2[which(geo_data$country == "Kosovo")] = "XK"
# ISO3 and IOS2 für Kosovo: https://www.wikidata.org/wiki/Q1246; abgerufen am 25.04.2019
geo_data$ISO3[which(geo_data$country == "St Martin")] = "MAF"
geo_data$ISO2[which(geo_data$country == "St Martin")] = "MF"
# ISO3 and IOS2 für St Martin: https://www.iso.org/obp/ui/#iso:code:3166:MF; abgerufen am 25.04.2019
# ISO for Channel Islands does not exist.
# Warning message will be taken care of.

geo_data = merge(geo_data, ccodes()[,c(2, 10)], by.x="ISO3", by.y="ISO3", all.x=T) # merge with ccodes()
detach("package:raster", unload=TRUE) # need to unload, otherwise dplyr::select will not work.
rm(countries)
# Information does not exist for: Channel Islands, Diamond Princess, Kosovo, MS Zaandam, St Martin.
# Get information manually: Channel Islands = Europe, Diamond Princess = NA, Kosovo = Europe, 
# MS Zaandam = NA (Kreuzfahrtschiff), St Martin = North America.
geo_data = geo_data%>%
  rename("country" = "country",
         "Continent" = "continent")
geo_data$Continent[which(geo_data$country == "Channel Islands")] = "Europe"
geo_data$Continent[which(geo_data$country == "Kosovo")] = "Europe"
geo_data$Continent[which(geo_data$country == "St Martin")] = "North America"

df_states = df_states%>%
  left_join(geo_data, by = "country") # Warning message can be ignored
rm(geo_data) # tidy up

## Getting the Population data for each country by ISO§
library("wbstats") # to get population data from Worldbank

countries_iso3 = unique(as.vector(df_states$ISO3))
pop_data = wb(country = countries_iso3, indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2018) 
# Population data from 2018 is being used. 2019 is not available, yet.
# Not Information is available for the following ISO3: VAT,ESH,AIA,BES,FLK,GUF,GLP,MTQ,MYT,MSR,REU,BLM,SPM
pop_data = pop_data%>% 
  select(iso3c, value)%>%
  rename("ISO3" = "iso3c",
         "Population" = "value")

pop_data = pop_data%>%
  add_row(ISO3 = "VAT", Population = 453) %>% # https://de.wikipedia.org/wiki/Vatikanstadt#Bevölkerung; abgerufen am 25.04.2019
  add_row(ISO3 = "ESH", Population = 597000) %>% # https://de.wikipedia.org/wiki/Westsahara; abgerufen am 25.04.2019
  add_row(ISO3 = "AIA", Population = 13572) %>% # https://de.wikipedia.org/wiki/Anguilla; abgerufen am 25.04.2019
  add_row(ISO3 = "BES", Population = 18413) %>% # https://de.wikipedia.org/wiki/Bonaire; abgerufen am 25.04.2019
  add_row(ISO3 = "FLK", Population = 2922) %>% # https://de.wikipedia.org/wiki/Falklandinseln; abgerufen am 25.04.2019
  add_row(ISO3 = "GUF", Population = 268700) %>% # https://de.wikipedia.org/wiki/Französisch-Guayana; abgerufen am 25.04.2019
  add_row(ISO3 = "GLP", Population = 390253) %>% # https://de.wikipedia.org/wiki/Guadeloupe; abgerufen am 25.04.2019
  add_row(ISO3 = "MTQ", Population = 372594) %>% # https://de.wikipedia.org/wiki/Martinique; abgerufen am 25.04.2019
  add_row(ISO3 = "MYT", Population = 256518) %>% # https://de.wikipedia.org/wiki/Mayotte; abgerufen am 25.04.2019
  add_row(ISO3 = "MSR", Population = 4922) %>% # https://de.wikipedia.org/wiki/Montserrat; abgerufen am 25.04.2019
  add_row(ISO3 = "REU", Population = 853659) %>% # https://de.wikipedia.org/wiki/Réunion; abgerufen am 25.04.2019
  add_row(ISO3 = "BLM", Population = 9961) %>% # https://de.wikipedia.org/wiki/Saint-Barthélemy_(Insel); abgerufen am 25.04.2019
  add_row(ISO3 = "SPM", Population = 5997) %>% # https://de.wikipedia.org/wiki/Saint-Pierre_und_Miquelon; abgerufen am 25.04.2019
  add_row(ISO3 = NA, Population = NA) # for Diamond Princess and MS Zaandam

df_states = df_states%>%
  left_join(pop_data, by = "ISO3") # Warning message can be ignored

detach("package:wbstats", unload=TRUE) # cleaning
rm(countries_iso3, pop_data) # cleaning

#calculate cumulative cases per 100k population.
df_states = df_states%>%
  mutate(cumulative100k = (cumulative/Population)*100000)

### -------------------------------------------------------------------------------------------------------------------
# Calculating growth factor for df_world
df_world = df_world%>%
  group_by(type)%>%
  mutate(gf = cumulative.world/lag(cumulative.world, n=1))%>%
  mutate(growth.factor.mean = zoo::rollapply(gf, 7, geometric.mean, fill=NA, align="right"))%>% # geometric rolling mean of past 7 days.
  mutate(doubling.time.mean = log(2)/log(growth.factor.mean)) # calculates mean doubling time of past 7 days
df_world$country = "World" # necessary for adding to a plot that contains country as variable

# Weltbevölkerung im Jahr 2018: 7,594 Mrd.; https://data.worldbank.org/region/world; abgerufen am 25.04,2019
df_world = df_world%>%
  mutate(cumulative100k = cumulative.world/(7.594*10^9)*100000)

# subbsetting the data.frame df_world by type
df_world_confirmed = df_world%>%
  filter(type == "confirmed")
df_world_death = df_world%>%
  filter(type == "death")
df_world_recovered = df_world%>%
  filter(type == "recovered")
### --------------------------------------------------------------------------------------------------------------------
## Centering Data for C6
## Data C6 contains the date at which 'C6: Stay at home requirements' were introduced in European countries.

# Load dataset from directory 'data'
df_c6 = read.csv(file = "data/c6_start_dates.csv")
df_c6 = df_c6 %>%
  rename("ISO3" = "X", "c6.start" = "X0")

df_states = df_states%>%
  left_join(df_c6, by="ISO3")

df_states = df_states%>%
  mutate(diff.c6 = difftime(date, as.Date(c6.start), units = "days"))


### -------------------------------------------------------------------------------------------------------------------
## Cumulative Number of confirmed cases/deaths and recovered world wide.
# on a linear scale
plot_world = ggplot()+
  geom_line(data = df_world_confirmed%>%filter(date <= "2020-04-27"), aes(x = date, y = cumulative100k), size = 1.5, color = 'blue')+
  geom_line(data = df_world_death%>%filter(date <= "2020-04-27"), aes(x = date, y = cumulative100k), size = 1.5, color = 'red')+
  geom_line(data = df_world_recovered%>%filter(date <= "2020-04-27"), aes(x = date, y = cumulative100k), size = 1.5, color = 'grey50')+
  labs(title = "World", subtitle = "Cumulative number of recorded cases per 100k people on linear scale", 
       x = "Date", y = "Cumulative Cases per 100k")+
  theme_bw()+
  theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))+
  
  geom_text(aes(x = as.Date("2020-04-15"), y = 35, label = "Confirmed"), color = 'blue', size = 7)+
  geom_text(aes(x = as.Date("2020-04-15"), y = 10, label = "Recovered"), color = 'grey50', size = 7)+
  geom_text(aes(x = as.Date("2020-04-15"), y = 3.5, label = "Deaths"), color = 'red', size = 7)
plot_world
ggsave("figures/Cases_world.pdf", plot = plot_world, width = 11, height = 8.5, units = "in")

# on a log scale
plot_world_log = ggplot()+
  geom_line(data = df_world_confirmed%>%filter(date <= "2020-04-27"), aes(x = date, y = cumulative100k), size = 1.5, color = 'blue')+
  geom_line(data = df_world_death%>%filter(date <= "2020-04-27"), aes(x = date, y = cumulative100k), size = 1.5, color = 'red')+
  geom_line(data = df_world_recovered%>%filter(date <= "2020-04-27"), aes(x = date, y = cumulative100k), size = 1.5, color = 'grey50')+
  labs(title = "World", subtitle = "Cumulative number of recorded cases per 100k people on log scale", 
       x = "Date", y = "Cumulative Cases per 100k")+
  theme_bw()+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  annotation_logticks(side="l") +
  theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))+
  
  geom_text(aes(x = as.Date("2020-04-15"), y = 35, label = "Confirmed"), color = 'blue', size = 7)+
  geom_text(aes(x = as.Date("2020-04-15"), y = 10, label = "Recovered"), color = 'grey50', size = 7)+
  geom_text(aes(x = as.Date("2020-04-15"), y = 3.5, label = "Deaths"), color = 'red', size = 7)
plot_world_log
ggsave("figures/Cases_world_log.pdf", plot = plot_world_log, width = 11, height = 8.5, units = "in")

### From now on the analysis focuses on Cases and Deaths. 

## Cumulative Cases for individual countries an log Scales
plot_cumulative_confirmed = df_states%>%
  group_by(country, type, Continent)%>%
  filter(country != "MS Zaandam" & country != "Diamond Princess")%>%
  filter(type == "confirmed")%>%
  filter(date <= "2020-04-27")%>%
  #filter(country != "US")%>%
  #filter(Continent == "Africa" | Continent == "Europe")%>%
  filter(cumulative >= 1)%>%
    ggplot(aes(date, cumulative100k, group = country))+
      geom_line(color = 'grey50')+
      scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                    labels = scales::trans_format("log10", scales::math_format(10^.x)))+
      annotation_logticks(side="l") +
      labs(title = "Confirmed Cases", subtitle = "Cumulative number of recorded cases per 100k people with min. of 1 case recorded", 
       x = "Date", y = "Cumulative Cases per 100k")+
      theme_bw()+
      theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))
  #facet_grid(cols = vars(Continent))

df_china_confirmed = df_states%>%
  filter(country == "China")%>%
  filter(type == "confirmed")%>%
  filter(cumulative >= 1)
 
plot_cumulative_confirmed = plot_cumulative_confirmed+
  geom_line(data = df_china_confirmed%>%filter(date <= "2020-04-27"), aes(x = date, y = cumulative100k), size = 1.5, color = 'black')+
  geom_text(aes(x = as.Date("2020-02-03"), y = 5, label = "China"), color = 'black', size = 7)+
  geom_line(data = df_world_confirmed%>%filter(date <= "2020-04-27"), aes(x = date, y = cumulative100k), size = 1.5, color = 'blue')+
  geom_text(aes(x = as.Date("2020-02-03"), y = 10^(-1), label = "World"), color = 'blue', size = 7)
  
  
plot_cumulative_confirmed
ggsave("figures/Cases_cumulative_confirmed.pdf", plot = plot_cumulative_confirmed, width = 11, height = 8.5, units = "in")

## Cumulative Deaths for individual countries an log Scales
plot_cumulative_death = df_states%>%
  group_by(country, type, Continent)%>%
  filter(country != "MS Zaandam" & country != "Diamond Princess")%>%
  filter(type == "death")%>%
  filter(date <= "2020-04-27")%>%
  #filter(country != "US")%>%
  #filter(Continent == "Africa" | Continent == "Europe")%>%
  filter(cumulative >= 1)%>%
  ggplot(aes(date, cumulative100k, group = country))+
  geom_line(color = 'grey50')+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  annotation_logticks(side="l") +
  theme_bw()+
  labs(title = "Confirmed Deaths", subtitle = "Cumulative number of recorded deaths per 100k people with min. of 1 case recorded", 
       x = "Date", y = "Cumulative Cases per 100k")+
  theme_bw()+
  theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))
#facet_grid(cols = vars(Continent))

df_china_confirmed = df_states%>%
  filter(country == "China")%>%
  filter(type == "death")%>%
  filter(cumulative >= 1)

plot_cumulative_death = plot_cumulative_death+
  geom_line(data = df_china_confirmed%>%filter(date <= "2020-04-27"), aes(x = date, y = cumulative100k), size = 1.5, color = 'black')+
  geom_text(aes(x = as.Date("2020-02-03"), y = 10^(-1), label = "China"), color = 'black', size = 7)+
  geom_line(data = df_world_death%>%filter(date <= "2020-04-27"), aes(x = date, y = cumulative100k), size = 1.5, color = 'red')+
  geom_text(aes(x = as.Date("2020-02-03"), y = 2*10^(-3), label = "World"), color = 'red', size = 7)
  

plot_cumulative_death
ggsave("figures/Cases_cumulative_deaths.pdf", plot = plot_cumulative_death, width = 11, height = 8.5, units = "in")

## Plot: from 100 confirmed cases on. Replication rate.
# generating a Dataframe for doubeling times
time_days = c(0:20)
dt2 = 100*2^(time_days/2)
dt4 = 100*2^(time_days/4)
dt8 = 100*2^(time_days/8)
dt16 = 100*2^(time_days/16)
dt32 = 100*2^(time_days/32)
dt64 = 100*2^(time_days/64)

df_doubling_time = data.frame(time_days, dt2, dt4, dt8, dt16, dt32, dt64)
df_doubling_time = df_doubling_time%>%
    pivot_longer(dt2:dt64, names_to="Days to double")
df_doubling_time$`Days to double` = factor(df_doubling_time$`Days to double`, levels = c("dt2", "dt4", "dt8", "dt16", "dt32", "dt64"),
                             labels=c("2", "4", "8", "16", "32", "64"))
rm(time_days, dt2, dt4, dt8, dt16, dt32, dt64)

plot_days_to_double_confirmed = df_states%>%
  group_by(country, type)%>%
  filter(cumulative >= 100)%>%
  filter(date <= "2020-04-27")%>%
  mutate(days = difftime(date, min(date), units = "days"))%>%
  filter(country != "MS Zaandam" & country != "Diamond Princess")%>%
  filter(type == "confirmed")%>%
    ggplot()+
      geom_path(aes(days, cumulative, group = country), lineend = "round", color='grey50', na.rm = TRUE)+
      xlim(0, 50)+
      scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
      annotation_logticks(side="l") +
      labs(x =" Days since 100 confirmed cases", y ="Cumulative number of confirmed cases",
           titel = "Confirmed Cases") +
      theme_bw()+
      geom_line(data = df_doubling_time, aes(time_days, value, group = `Days to double`, color=`Days to double`),
              size = 1.5, linetype = 5)+
      scale_color_viridis_d()+
      facet_grid(cols = vars(Continent))

plot_days_to_double_confirmed 
### -------------------------------------------------------------------------------------------------------------------
## Calculate growth factor on a day-to-day basis for df_states
df_states_wider = df_states%>%
  select(country, date, type, cumulative)%>%
  pivot_wider(names_from = c(country, type), values_from = cumulative)

growth.factor = matrix(data = NA, nrow = (dim(df_states_wider)[1]-1), ncol=(dim(df_states_wider)[2]-1))
dim(growth.factor)

for (i in 1:ncol(growth.factor)){
  i = i+1
  for (j in 1:nrow(growth.factor)){
    k = j+1
    gf = as.numeric(df_states_wider[k,i]/df_states_wider[j,i])
    growth.factor[j, (i-1)] = gf
  }
}
rm(i, j, k, gf)

last_date = matrix(data = NA, nrow = 1, ncol=(dim(df_states_wider)[2]-1)) # for the last date, there is no doubling time calculable 
growth.factor = rbind(last_date, growth.factor)
rm(last_date)

dates =  df_states_wider%>%
  select(date)%>%
  as.vector()
growth.factor = cbind(dates, growth.factor)

colnames.df_states_wider = colnames(df_states_wider)
colnames(growth.factor) = colnames.df_states_wider
rm(colnames.df_states_wider, df_states_wider)

df_growth.factor = as.data.frame(growth.factor)

df_growth.factor = df_growth.factor%>%
  pivot_longer(-date, names_to = c("country", "type"), names_sep = "_", values_to = "gf")
  
df_states = df_states%>%
  left_join(y = df_growth.factor, by = c("country", "type", "date"))
rm(df_growth.factor)

# df_states now contains the growth facter. 
# From this the 7 day rolling geometric mean will be calculated.
df_states$gf[which(is.nan(df_states$gf))] = NA
df_states$gf[which(is.infinite(df_states$gf))] = NA

df_states = df_states%>%
  group_by(country, type)%>%
  mutate(growth.factor.mean = zoo::rollapply(gf, 7, geometric.mean, fill=NA, align="right"))%>% # geometric rolling mean of past 7 days.
  mutate(doubling.time.mean = log(2)/log(growth.factor.mean)) # calculates mean doubling time of past 7 days


#### -------------------------------------------------------------------------------------------------------------------
### Plots of GROWTH FACTORS
## Plot for growth factor of confirmed cases
plot_growth.fractor_confirmed = df_states%>%
  filter(type == "confirmed")%>%
  filter(cumulative >= 50)%>%
  filter(growth.factor.mean<=1.75)%>%
  filter(date <= "2020-04-27")%>%
  filter(country != "MS Zaandam" & country != "Diamond Princess" )%>%
    ggplot(aes(x = date, y = growth.factor.mean))+
    ylim(0.95, 1.75)+
    geom_line(aes(group = country), color = 'grey50')+
    labs(title = "Growth factors: Recorded Cases", subtitle = "7-day rolling geometric mean growth factors of all countries with more than 50 cases recorded", 
         x = "Date", y = "growth factor")+
    theme_bw()+
    theme(plot.title = element_text(size = 25),
          plot.subtitle = element_text(size = 15),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15))

# adding World growth.factor to the plot
plot_growth.fractor_confirmed = plot_growth.fractor_confirmed+ 
  geom_line(data = df_world_confirmed%>%filter(date <= "2020-04-27"), aes(x = date, y = growth.factor.mean), 
            color = 'blue', size = 1.5)+
  geom_text(aes(x = as.Date("2020-01-28"), y = 1.5, label = "World"), color = 'blue', size = 7)
plot_growth.fractor_confirmed
ggsave("GF_confirmed.pdf", plot = plot_growth.fractor_confirmed, width = 11, height = 8.5, units = "in")

## Plot for growth factor of death cases
plot_growth.fractor_death = df_states%>%
  filter(type == "death")%>%
  filter(cumulative >= 20)%>%
  filter(growth.factor.mean<=1.75)%>%
  filter(date <= "2020-04-27")%>%
  filter(country != "MS Zaandam" & country != "Diamond Princess" )%>%
  ggplot(aes(x = date, y = growth.factor.mean))+
  geom_line(aes(group = country), color = 'grey50')+
    labs(title = "Growth factors: Recorded Deaths", subtitle = "7-day rolling geometric mean of growth factors of all countries with more than 20 deaths recorded", 
         x = "Date", y = "growth factor")+
    theme_bw()+
    ylim(0.95, 1.75)+
    theme(plot.title = element_text(size = 25),
          plot.subtitle = element_text(size = 15),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15))

# adding World growth.factor to the plot
plot_growth.fractor_death = plot_growth.fractor_death+ 
  geom_line(data = df_world_death%>%filter(date <= "2020-04-27"), aes(x = date, y = growth.factor.mean), 
            color = 'red', size = 1.5)+
  geom_text(aes(x = as.Date("2020-01-28"), y = 1.45, label = "World"), color = 'red', size = 7)
plot_growth.fractor_death
ggsave("GF_deaths.pdf", plot = plot_growth.fractor_death, width = 11, height = 8.5, units = "in")

## Plot for growth factor of recovered cases
plot_growth.fractor_recovered = df_states%>%
  filter(type == "recovered")%>%
  filter(cumulative >= 20)%>%
  filter(growth.factor.mean<=1.75)%>%
  filter(date <= "2020-04-27")%>%
  filter(country != "MS Zaandam" & country != "Diamond Princess" )%>%
  ggplot(aes(x = date, y = growth.factor.mean))+
  geom_line(aes(group = country), color = 'grey50')+
  labs(title = "Growth factors: Recorded Recovered", subtitle = "7-day rolling geometric mean of growth factors of all countries with more than 20 recovered cases", 
       x = "Date", y = "growth factor")+
  theme_bw()+
  ylim(0.95, 1.75)+
  theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))

# adding World growth.factor to the plot
plot_growth.fractor_recovered = plot_growth.fractor_recovered+ 
  geom_line(data = df_world_recovered%>%filter(date <= "2020-04-27"), aes(x = date, y = growth.factor.mean), 
            color = 'black', size = 1.5)+
  geom_text(aes(x = as.Date("2020-01-28"), y = 1.45, label = "World"), color = 'black', size = 7)
plot_growth.fractor_recovered

#### -------------------------------------------------------------------------------------------------------------------
### Plots of DOUBLING TIMES
## Plot for Doubling Times of confirmed cases
plot_doubling.time_confirmed = df_states%>%
  filter(type == "confirmed")%>%
  filter(cumulative >= 50)%>%
  filter(date <= "2020-04-27")%>%
  filter(country != "MS Zaandam" & country != "Diamond Princess")%>%
    ggplot(aes(x = date, y = doubling.time.mean))+
      geom_line(aes(group= country), color = 'grey50')+
      ylim(0, 150)+
      labs(title = "Doubling Times: Recorded Cases", subtitle = "7-day rolling geometric mean of doubling time of all countries with more than 50 cases recorded", 
         x = "Date", y = "Days")+
      theme_bw()+
      theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))

plot_doubling.time_confirmed = plot_doubling.time_confirmed+
  geom_line(data = df_world_confirmed%>%filter(date <= "2020-04-27"), aes(x = date, y = doubling.time.mean), 
            color = 'blue', size = 1.5)+
  geom_text(aes(x = as.Date("2020-01-28"), y = 12.5, label = "World"), color = 'blue', size = 7)
plot_doubling.time_confirmed
ggsave("DT_confirmed.pdf", plot = plot_doubling.time_confirmed, width = 11, height = 8.5, units = "in")

## Plot for Doubling Times of confirmed death
plot_doubling.time_deaths = df_states%>%
  filter(type == "death")%>%
  filter(cumulative >= 20)%>%
  filter(date <= "2020-04-27")%>%
  filter(country != "MS Zaandam" & country != "Diamond Princess")%>%
  ggplot(aes(x = date, y = doubling.time.mean))+
  geom_line(aes(group= country), color = 'grey50')+
  ylim(0, 150)+
  labs(title = "Doubling Times: Recorded Deaths", subtitle = "7-day rolling geometric mean of doubling time of all countries with more than 20 deaths recorded", 
       x = "Date", y = "Days")+
  theme_bw()+
  theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))

plot_doubling.time_deaths = plot_doubling.time_deaths+
  geom_line(data = df_world_death%>%filter(date <= "2020-04-27"), aes(x = date, y = doubling.time.mean), 
            color = 'red', size = 1.5)+
  geom_text(aes(x = as.Date("2020-01-28"), y = 12.5, label = "World"), color = 'red', size = 7)
plot_doubling.time_deaths
ggsave("DT_deaths.pdf", plot = plot_doubling.time_deaths, width = 11, height = 8.5, units = "in")

## Plot for Doubling Times of confirmed death
plot_doubling.time_recovered = df_states%>%
  filter(type == "recovered")%>%
  filter(cumulative >= 20)%>%
  filter(date <= "2020-04-27")%>%
  filter(country != "MS Zaandam" & country != "Diamond Princess")%>%
  ggplot(aes(x = date, y = doubling.time.mean))+
  geom_line(aes(group= country), color = 'grey50')+
  ylim(0, 150)+
  labs(title = "Doubling Times: Recovered Cases", subtitle = "7-day rolling geometric mean of doubling time of all countries with more than 20 recovered cases", 
       x = "Date", y = "Days")+
  theme_bw()+
  theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))

plot_doubling.time_recovered = plot_doubling.time_recovered+
  geom_line(data = df_world_death%>%filter(date <= "2020-04-27"), aes(x = date, y = doubling.time.mean), 
            color = 'black', size = 1.5)+
  geom_text(aes(x = as.Date("2020-01-28"), y = 12.5, label = "World"), color = 'black', size = 7)
plot_doubling.time_recovered


### -------------------------------------------------------------------------------------------------------------------
## Plot growth factor for european countries, since C6: Introduction of Stay at Home Requirements
plot_growth.factor_C6_confirmed = df_states%>%
  filter(type == "confirmed")%>%
  filter(diff.c6 >= 0)%>%
  filter(doubling.time.mean < Inf)%>%
  filter(country != "MS Zaandam" & country != "Diamond Princess")%>%
  ggplot(aes(x = diff.c6, y = growth.factor.mean))+
    geom_line(aes(group= country), color = "blue")+
    ylim(0.95, 1.75)+
    labs(title = "Growth Factor: Recorded European Cases", subtitle = "7-day rolling geometric mean of growth factor of all euorpean countries
since introducing \"Stay at Home\" - Requirements",
       x = "Days since C6", y = "growth factor")+
    theme_bw()+
    theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))

plot_growth.factor_C6_deaths = df_states%>%
  filter(type == "death")%>%
  filter(diff.c6 >= 0)%>%
  filter(doubling.time.mean < Inf)%>%
  filter(country != "MS Zaandam" & country != "Diamond Princess")%>%
  ggplot(aes(x = diff.c6, y = growth.factor.mean))+
  geom_line(aes(group= country), color = 'red')+
  ylim(0.95, 1.75)+
  labs(title = "Growth Factor: Recorded European Deaths", subtitle = "7-day rolling geometric mean of growth factor of all euorpean countries
since introducing \"Stay at Home\" - Requirements",
       x = "Days since C6", y = "growth factor")+
  theme_bw()+
  theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))

plot_c6_gf = gridExtra::grid.arrange(plot_growth.factor_C6_confirmed, plot_growth.factor_C6_deaths, ncol=2)
ggsave("plot_c6_gf.pdf", plot = plot_c6_gf, width = 17, height = 9, units = "in")

plot_growth.factor_C6_recovered = df_states%>%
  filter(type == "recovered")%>%
  filter(diff.c6 >= 0)%>%
  filter(doubling.time.mean < Inf)%>%
  filter(country != "MS Zaandam" & country != "Diamond Princess")%>%
  ggplot(aes(x = diff.c6, y = growth.factor.mean))+
  geom_line(aes(group= country), color = 'black')+
  ylim(0.95, 1.75)+
  labs(title = "Growth Factor: Recorded European Recovered", subtitle = "7-day rolling geometric mean of growth factor of all euorpean countries
since introducing \"Stay at Home\" - Requirements",
       x = "Days since C6", y = "growth factor")+
  theme_bw()+
  theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))
plot_growth.factor_C6_recovered

## Plot doubling time for european countries, since C6: Introduction of Stay at Home Requirements

plot_doubling.time_C6_confirmed = df_states%>%
  filter(type == "confirmed")%>%
  filter(diff.c6 >= 0)%>%
  filter(doubling.time.mean < Inf)%>%
  filter(country != "MS Zaandam" & country != "Diamond Princess")%>%
  ggplot(aes(x = diff.c6, y = doubling.time.mean))+
    geom_line(aes(group= country), color = 'blue')+
    ylim(0, 150)+
    labs(title = "Doubling Time: Recorded European Cases", subtitle = "7-day rolling geometric mean of doubling time of all euorpean countries
since introducing \"Stay at Home\" - Requirements",
       x = "Days since C6", y = "Days to double")+
    theme_bw()+
    theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))

plot_doubling.time_C6_death = df_states%>%
  filter(type == "death")%>%
  filter(diff.c6 >= 0)%>%
  filter(doubling.time.mean < Inf)%>%
  filter(country != "MS Zaandam" & country != "Diamond Princess")%>%
  ggplot(aes(x = diff.c6, y = doubling.time.mean))+
  geom_line(aes(group= country), color = 'red')+
  ylim(0, 150)+
  labs(title = "Doubling Time: Recorded European Deaths", subtitle = "7-day rolling geometric mean of doubling time of all euorpean countries
since introducing \"Stay at Home\" - Requirements",
       x = "Days since C6", y = "Days to double")+
  theme_bw()+
  theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))

plot_c6_dt = gridExtra::grid.arrange(plot_doubling.time_C6_confirmed, plot_doubling.time_C6_death, ncol=2)
ggsave("plot_c6_dt.pdf", plot = plot_c6_dt, width = 17, height = 9, units = "in")

plot_doubling.time_C6_recovered = df_states%>%
  filter(type == "recovered")%>%
  filter(diff.c6 >= 0)%>%
  filter(doubling.time.mean < Inf)%>%
  filter(country != "MS Zaandam" & country != "Diamond Princess")%>%
  ggplot(aes(x = diff.c6, y = doubling.time.mean))+
  geom_line(aes(group= country), color = 'black')+
  ylim(0, 150)+
  labs(title = "Doubling Time: Recorded European Recovered", subtitle = "7-day rolling geometric mean of doubling time of all euorpean countries
since introducing \"Stay at Home\" - Requirements",
       x = "Days since C6", y = "Days to double")+
  theme_bw()+
  theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))
plot_doubling.time_C6_recovered


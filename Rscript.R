
#.................................. Load data from package ..........................................
library(coronavirus)
data("coronavirus")
data("covid_iran") # not found
data("covid_south_korea") # not found

#..............................Load data manually from github........................................
#..................................also adding Italy.................................................
covid_iran <- read.csv("https://raw.githubusercontent.com/RamiKrispin/coronavirus-csv/master/iran/covid_iran_long.csv",
                            stringsAsFactors = FALSE)

covid_south_korea <- read.csv("https://raw.githubusercontent.com/RamiKrispin/coronavirus-csv/master/south_korea/covid_south_korea_long.csv",
                          stringsAsFactors = FALSE)

covid_italy <- read.csv("https://raw.githubusercontent.com/RamiKrispin/coronavirus-csv/master/italy/covid_italy_long.csv",
                       stringsAsFactors = FALSE)


#...................................Updating datasets...............................................
update_datasets <- function(silence = FALSE){
  flag <- FALSE
  
  coronavirus_current <- coronavirus::coronavirus
  iran_current <- covid_iran
  sk_current <- covid_south_korea
  it_current <- covid_italy
  
  
  coronavirus_git <- utils::read.csv("https://raw.githubusercontent.com/RamiKrispin/coronavirus-csv/master/coronavirus_dataset.csv",
                                     stringsAsFactors = FALSE)
  
  iran_git <- utils::read.csv("https://raw.githubusercontent.com/RamiKrispin/coronavirus-csv/master/iran/covid_iran_long.csv",
                              stringsAsFactors = FALSE)
  
  sk_git <- utils::read.csv("https://raw.githubusercontent.com/RamiKrispin/coronavirus-csv/master/south_korea/covid_south_korea_long.csv",
                            stringsAsFactors = FALSE)
  
  it_git <- utils::read.csv("https://raw.githubusercontent.com/RamiKrispin/coronavirus-csv/master/italy/covid_italy_long.csv",
                           stringsAsFactors = FALSE)
  
  coronavirus_git$date <- base::as.Date(coronavirus_git$date)
  iran_git$date <- base::as.Date(iran_git$date)
  sk_git$date <- base::as.Date(sk_git$date)
  it_git$date <- base::as.Date(it_git$date)
  
  
  if(!base::identical(coronavirus_git, coronavirus_current)){
    if(base::nrow(coronavirus_git) > base::nrow(coronavirus_current)){
      flag <- TRUE
    }
  }
  
  if(!base::identical(iran_git, iran_current)){
    if(base::nrow(iran_git) > base::nrow(iran_current)){
      flag <- TRUE
    }
  }
  
  if(!base::identical(sk_git, sk_current)){
    if(base::nrow(sk_git) > base::nrow(sk_current)){
      flag <- TRUE
    }
  }
  
  if(!base::identical(it_git, it_current)){
    if(base::nrow(it_git) > base::nrow(it_current)){
      flag <- TRUE
    }
  }
  
  if(flag){
    if(!silence){
      q <- base::tolower(base::readline("Updates are available on the coronavirus Dev version, do you want to update? n/Y"))
    } else {
      q <- "y"
    }
    if(q == "y" | q == "yes"){
      
      base::tryCatch(
        expr = {
          devtools::install_github("RamiKrispin/coronavirus")
          
          base::message("The data was refresed, please restart your session to have the new data available")
        },
        error = function(e){
          message('Caught an error!')
          print(e)
        },
        warning = function(w){
          message('Caught an warning!')
          print(w)
        }
        
      )
    }
  } else {
    base::message("No updates are available")
  }
  
  
}
.rs.restartR()

# Load Packages after restart of R
library(dplyr)
library(tidyr)
library(lubridate)
library(plotly)
library(viridis)
library(coronavirus)
library(psych)
library(zoo)
data("coronavirus")

####################################### Inspect dataset ############################################
setwd("/Users/lorenzmihatsch/Desktop/Statistik/Corona/Presentation")
# copy dataset to working data frame & convert empty cells to NAs
df_all <- coronavirus %>% mutate_if(is.character, list(~na_if(., "")))
df_ir <- covid_iran %>% mutate_if(is.character, list(~na_if(., "")))
df_sk <- covid_south_korea %>% mutate_if(is.character, list(~na_if(., "")))
df_it <- covid_italy %>% mutate_if(is.character, list(~na_if(., "")))

# convert Province.State, Country.State, type from character to factor
#df_all <- mutate_if(df_all, is.character, as.factor)
#df_ir <- mutate_if(df_ir, is.character, as.factor)
#df_sk <- mutate_if(df_sk, is.character, as.factor)
#df_it <- mutate_if(df_it, is.character, as.factor)

str(df_all)
summary(df_all)

missing_col <- sapply(df_all, function(x) sum(is.na(x) )) / nrow(df_all)
missing_col[missing_col > 0]
rm(missing_col)

########################### Begin Descriptive Analysis #############################################

# Reason: Australia and China are only given by thier Provinces/Counties
# Canada is given by its Counties and additionally as Country without provinces
# Denmark, France, Netherlands and UK are given as Country and as some of their 
# former/current colonies.

#df_all = df_all%>%
  # group_by(Province.State, Country.Region, type)%>%
  # mutate(cumulative = cumsum(cases))%>% # cumsum: Variable with cumulative sum.
  # ungroup()
  
  # Cumsum for Countries with Province.States != NA are only given by their Province.State,
  # not for the whole country.

df_provinces = df_all%>%
  na.omit(cols="Province.State")%>%
  filter(Country.Region == "China" | Country.Region == "Australia")%>% # Provinces of Canada not included
  group_by(Country.Region, date, type)%>%
  summarise(cases = sum(cases))%>% # by summarizing all other variables are lost.
  ungroup()%>%
  group_by(Country.Region, type)%>%
  mutate(cumulative = cumsum(cases))%>% # cumsum: Variable with cumulative sum.
  ungroup()
  
df_colonies = df_all%>%
  na.omit(cols="Province.State")%>%
  filter(Country.Region != "China" & Country.Region != "Australia" & Country.Region != "Canada")%>%
  select(-Country.Region)%>%
  rename("Country.Region"="Province.State")%>%
  group_by(Country.Region, type)%>%
  mutate(cumulative = cumsum(cases))%>% # cumsum: Variable with cumulative sum.
  ungroup()

df_canada = df_all%>%
  filter(Country.Region == "Canada")%>%
  group_by(Country.Region, date, type)%>%
  summarise(cases = sum(cases))%>% # by summarizing all other variables are lost.
  ungroup()%>%
  group_by(Country.Region, type)%>%
  mutate(cumulative = cumsum(cases))%>% # cumsum: Variable with cumulative sum.
  ungroup()

df_states = df_all %>%
  filter(Country.Region != "Canada")%>%
  filter(is.na(Province.State))%>%
  select(-Province.State)%>%
  group_by(Country.Region, type)%>%
  mutate(cumulative = cumsum(cases))%>% # cumsum: Variable with cumulative sum.
  ungroup()

df_states = df_states%>%
  bind_rows(df_provinces)%>%
  bind_rows(df_colonies)%>%
  bind_rows(df_canada)%>%
  select(-Lat, -Long)
rm(df_provinces, df_colonies, df_canada, df_all) # tidy up

# This df_world data.frame containf the cumulative world data
df_world = df_states%>%
  select(Country.Region, date, cases, type)%>%
  pivot_wider(names_from = c(Country.Region), values_from = cases)%>% #changing Country.Region to Variables
  ungroup()%>%
  mutate(cases.world = rowSums(.[3:ncol(.)]))%>% #Sum above all countries
  select(date, type, cases.world)%>%
  group_by(type)%>%
  mutate(cumulative.world = cumsum(cases.world)) # Cumulative sum over time


###-----------------------------------------------------------------------------------------
## Plot: Cummulative cases and types vs date for all countries
plot_cumulative_by_time_confirmed = df_states%>%
  group_by(Country.Region, type)%>%
  filter(type == "confirmed")%>%
  #filter(cumulative >=1)%>%
    ggplot(aes(date, cumulative, fill = Country.Region))+
      geom_line()+
      labs(x = "Cumulative number of confirmed cases")+
      #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
      #labels = trans_format("log10", math_format(10^.x)))+
      #annotation_logticks(side="l") +
      theme_bw()
plot_cumulative_by_time_confirmed

plot_cumulative_by_time_death = df_states%>%
  group_by(Country.Region, type)%>%
  filter(type == "death")%>%
    ggplot(aes(date, cumulative, fill = Country.Region))+
      geom_line()+
      labs(x = "Cumulative number of death cases")+
      theme_bw()
plot_cumulative_by_time_death

plot_cumulative_by_time_recovered = df_states%>%
  group_by(Country.Region, type)%>%
  filter(type == "recovered")%>%
    ggplot(aes(date, cumulative, fill = Country.Region))+
      geom_line()+
      labs(x = "Cumulative number of recovered cases")+
      theme_bw()
plot_cumulative_by_time_recovered

## Plot: by continent
countries <- unique(df_states$Country.Region) # Countries from the corona dataframe.
geo_data <- data.frame(country = countries) # as data.frane

library(countrycode) # to get the iso2c code
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
  rename("Country.Region" = "country",
         "Continent" = "continent")
geo_data$Continent[which(geo_data$Country.Region == "Channel Islands")] = "Europe"
geo_data$Continent[which(geo_data$Country.Region == "Kosovo")] = "Europe"
geo_data$Continent[which(geo_data$Country.Region == "St Martin")] = "North America"

df_states = df_states%>%
  left_join(geo_data, by = "Country.Region") # Warning message can be ignored
rm(geo_data)

## Getting the Population data for each country by ISO§
library("wbstats") # to get population data from Worldbank

countries_iso3 = unique(as.vector(df_states_wb$ISO3))
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


plot_cumulative_by_time_by_continent_confirmed = df_states%>%
  group_by(Country.Region, type, Continent)%>%
  filter(Country.Region != "MS Zaandam" & Country.Region != "Diamond Princess")%>%
  filter(type == "confirmed")%>%
  filter(Country.Region != "US")%>%
  #filter(Continent == "Africa" | Continent == "Europe")%>%
  filter(cumulative >= 1)%>%
    ggplot(aes(date, cumulative, group = Country.Region))+
      geom_line(aes(color = Continent))+
      #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    #labels = trans_format("log10", math_format(10^.x)))+
      #annotation_logticks(side="l") +
      theme_bw()+
  facet_grid(cols = vars(Continent))

plot_cumulative_by_time_by_continent_confirmed

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
  group_by(Country.Region, type)%>%
  filter(cumulative >= 100)%>%
  mutate(days = difftime(date, min(date), units = "days"))%>%
  filter(Country.Region != "MS Zaandam" & Country.Region != "Diamond Princess")%>%
  filter(type == "confirmed")%>%
    ggplot()+
      geom_path(aes(days, cumulative, group = Country.Region), lineend = "round", color='grey50', na.rm = TRUE)+
      xlim(0, 30)+
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x)))+
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
  select(Country.Region, date, type, cumulative)%>%
  pivot_wider(names_from = c(Country.Region, type), values_from = cumulative)

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
  pivot_longer(-date, names_to = c("Country.Region", "type"), names_sep = "_", values_to = "gf")
  
df_states = df_states%>%
  left_join(y = df_growth.factor, by = c("Country.Region", "type", "date"))
rm(df_growth.factor)

# df_states now contains the growth facter. 
# From this the 7 day rolling geometric mean will be calculated.
df_states$gf[which(is.nan(df_states$gf))] = NA
df_states$gf[which(is.infinite(df_states$gf))] = NA

df_states = df_states%>%
  group_by(Country.Region, type)%>%
  mutate(growth.factor.mean = zoo::rollapply(gf, 7, geometric.mean, fill=NA, align="right"))%>% # geometric rolling mean of past 7 days.
  mutate(doubling.time.mean = log(2)/log(growth.factor.mean)) # calculates mean doubling time of past 7 days
### -------------------------------------------------------------------------------------------------------------------
# Calculating growth factor for df_world
df_world = df_world%>%
  group_by(type)%>%
  mutate(gf = cumulative.world/lag(cumulative.world, n=1))%>%
  mutate(growth.factor.mean = zoo::rollapply(gf, 7, geometric.mean, fill=NA, align="right"))%>% # geometric rolling mean of past 7 days.
  mutate(doubling.time.mean = log(2)/log(growth.factor.mean)) # calculates mean doubling time of past 7 days
df_world$Country.Region = "World" # necessary for adding to a plot that contains Country.Region as variable

# subbsetting the data.frame df_world by type
df_world_confirmed = df_world%>%
  filter(type == "confirmed")
df_world_death = df_world%>%
  filter(type == "death")
df_world_recovered = df_world%>%
  filter(type == "recovered")

#### -------------------------------------------------------------------------------------------------------------------
### Plots of GROWTH FACTORS
## Plot for growth factor of confirmed cases
plot_growth.fractor_confirmed = df_states%>%
  filter(type == "confirmed")%>%
  filter(cumulative >= 50)%>%
  filter(Country.Region != "MS Zaandam" & Country.Region != "Diamond Princess" )%>%
    ggplot(aes(x = date, y = growth.factor.mean))+
    geom_line(aes(group = Country.Region), color = 'grey50')+
    labs(title = "Growth factors: Recorded Cases", subtitle = "7-day rolling geometric mean growth factors of all countries with more than 50 cases recorded", 
         x = "Date", y = "growth factor")+
    theme_bw()+
    theme(plot.title = element_text(size = 25),
          plot.subtitle = element_text(size = 15),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15))

# adding World growth.factor to the plot
plot_growth.fractor_confirmed = plot_growth.fractor_confirmed+ 
  geom_line(data = df_world_confirmed, aes(x = date, y = growth.factor.mean), 
            color = 'red', size = 1.5)+
  geom_text(aes(x = as.Date("2020-01-28"), y = 1.5, label = "World"), color = 'red', size = 7)
plot_growth.fractor_confirmed
ggsave("GF_confirmed.pdf", plot = plot_growth.fractor_confirmed, width = 11, height = 8.5, units = "in")

## Plot for growth factor of death cases
plot_growth.fractor_death = df_states%>%
  filter(type == "death")%>%
  filter(cumulative >= 20)%>%
  filter(Country.Region != "MS Zaandam" & Country.Region != "Diamond Princess" )%>%
  ggplot(aes(x = date, y = growth.factor.mean))+
  geom_line(aes(group = Country.Region), color = 'grey50')+
    labs(title = "Growth factors: Recorded Deaths", subtitle = "7-day rolling geometric mean of growth factors of all countries with more than 20 deaths recorded", 
         x = "Date", y = "growth factor")+
    theme_bw()+
    theme(plot.title = element_text(size = 25),
          plot.subtitle = element_text(size = 15),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15))

# adding World growth.factor to the plot
plot_growth.fractor_death = plot_growth.fractor_death+ 
  geom_line(data = df_world_death, aes(x = date, y = growth.factor.mean), 
            color = 'black', size = 1.5)+
  geom_text(aes(x = as.Date("2020-01-28"), y = 1.45, label = "World"), color = 'black', size = 7)
plot_growth.fractor_death
ggsave("GF_deaths.pdf", plot = plot_growth.fractor_death, width = 11, height = 8.5, units = "in")

## Plot for growth factor of recovered cases
{
plot_growth.fractor_recovered = df_states%>%
    filter(type == "recovered")%>%
    filter(cumulative >= 30)%>%
    filter(Country.Region != "MS Zaandam" & Country.Region != "Diamond Princess" )%>%
    filter(growth.factor.mean >= .8)%>%
    ggplot(aes(x = date, y = growth.factor.mean))+
    geom_line(aes(group = Country.Region), color = 'grey50')+
    theme_bw()+
    labs(x = "Date", y = "day-to-day growth rate")
  plot_growth.fractor_death
  
# adding World growth.factor to the plot
  plot_growth.fractor_recovered = plot_growth.fractor_recovered+ 
    geom_line(data = df_world_recovered, aes(x = date, y = growth.factor.mean), 
              color = 'blue', size = 1.5)
}

#### -------------------------------------------------------------------------------------------------------------------
### Plots of DOUBLING TIMES
## Plot for Doubling Times of confirmed cases
plot_doubling.time_confirmed = df_states%>%
  filter(type == "confirmed")%>%
  filter(cumulative >= 50)%>%
  filter(Country.Region != "MS Zaandam" & Country.Region != "Diamond Princess")%>%
    ggplot(aes(x = date, y = doubling.time.mean))+
      geom_line(aes(group= Country.Region), color = 'grey50')+
      ylim(0, 150)+
      labs(title = "Doubling Times: Recorded Cases", subtitle = "7-day rolling geometric mean of doubling time of all countries with more than 50 cases recorded", 
         x = "Date", y = "Days")+
      theme_bw()+
      theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))

plot_doubling.time_confirmed = plot_doubling.time_confirmed+
  geom_line(data = df_world_confirmed, aes(x = date, y = doubling.time.mean), 
            color = 'red', size = 1.5)+
  geom_text(aes(x = as.Date("2020-01-28"), y = 12.5, label = "World"), color = 'red', size = 7)
plot_doubling.time_confirmed
ggsave("DT_confirmed.pdf", plot = plot_doubling.time_confirmed, width = 11, height = 8.5, units = "in")

## Plot for Doubling Times of confirmed death
plot_doubling.time_deaths = df_states%>%
  filter(type == "death")%>%
  filter(cumulative >= 20)%>%
  filter(Country.Region != "MS Zaandam" & Country.Region != "Diamond Princess")%>%
  ggplot(aes(x = date, y = doubling.time.mean))+
  geom_line(aes(group= Country.Region), color = 'grey50')+
  ylim(0, 100)+
  labs(title = "Doubling Times: Recorded Deaths", subtitle = "7-day rolling geometric mean of doubling time of all countries with more than 20 deaths recorded", 
       x = "Date", y = "Days")+
  theme_bw()+
  theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))

plot_doubling.time_deaths = plot_doubling.time_deaths+
  geom_line(data = df_world_death, aes(x = date, y = doubling.time.mean), 
            color = 'black', size = 1.5)+
  geom_text(aes(x = as.Date("2020-01-28"), y = 12.5, label = "World"), color = 'black', size = 7)
plot_doubling.time_deaths
ggsave("DT_deaths.pdf", plot = plot_doubling.time_deaths, width = 11, height = 8.5, units = "in")

### -------------------------------------------------------------------------------------------------------------------
## Plot for growth factor of confirmed cases centered at 50 confirmed cases
plot_growth.rate_confirmed_centered50 = df_states%>%
  group_by(Country.Region, type)%>%
  filter(cumulative >= 50)%>%
  mutate(days = difftime(date, min(date), units = "days"))%>%
  filter(Country.Region != "MS Zaandam" & Country.Region != "Diamond Princess" & Country.Region != "China")%>%
  filter(type == "confirmed")%>%
    ggplot(aes(x = days, y = growth.factor.mean))+
    geom_line(aes(group = Country.Region), color = 'grey50')+
    theme_bw()+
    labs(x = "Date", y = "day-to-day growth rate")

plot_growth.rate_death_centered20 = df_states%>%
  group_by(Country.Region, type)%>%
  filter(cumulative >= 20)%>%
  mutate(days = difftime(date, min(date), units = "days"))%>%
  filter(Country.Region != "MS Zaandam" & Country.Region != "Diamond Princess" & Country.Region != "China")%>%
  filter(type == "death")%>%
  ggplot(aes(x = days, y = growth.factor.mean))+
  geom_line(aes(group = Country.Region), color = 'grey50')+
  theme_bw()+
  labs(x = "Date", y = "day-to-day growth rate")

plot_growth.rate_recovered_centered20 = df_states%>%
  group_by(Country.Region, type)%>%
  filter(cumulative >= 20)%>%
  mutate(days = difftime(date, min(date), units = "days"))%>%
  filter(Country.Region != "MS Zaandam" & Country.Region != "Diamond Princess" & Country.Region != "China")%>%
  filter(type == "recovered")%>%
  ggplot(aes(x = days, y = growth.factor.mean))+
  geom_line(aes(group = Country.Region), color = 'grey50')+
  theme_bw()+
  labs(x = "Date", y = "day-to-day growth rate")



plot_cases_germany = df_states%>%
  filter(Country.Region == "Germany")%>%
    ggplot(aes(x = date, y = cases))+
    geom_bar(stat = "identity")




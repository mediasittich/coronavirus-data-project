
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
library(coronavirus) #
data("coronavirus")

####################################### Inspect dataset ############################################

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

  # In df_states the lat and long for  is now missing.
  # data is obtained manualle from https://github.com/MISP/misp-dashboard/blob/master/data/country_code_lat_long.json

 # df_states$Lat[which(df_states$Country.Region == "Australia")] = -27.0000
 # df_states$Long[which(df_states$Country.Region == "Australia")] = 133.0000

 # df_states$Lat[which(df_states$Country.Region == "Canada")] = 60.0000
 # df_states$Long[which(df_states$Country.Region == "Canada")] = -95.0000

 # df_states$Lat[which(df_states$Country.Region == "China")] = 35.0000
 # df_states$Long[which(df_states$Country.Region == "China")] = 105.0000


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

geo_data$ISO3 <- countrycode(sourcevar = countries, origin = "country.name", destination = "iso3c") # get iso2c
geo_data$ISO2 <- countrycode(sourcevar = countries, origin = "country.name", destination = "iso2c")

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
  left_join(geo_data, by = "Country.Region")
rm(geo_data)

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

## Calculate time to double on a day-to-day basis

df_states_wider = df_states%>%
  select(Country.Region, date, type, cumulative)%>%
  pivot_wider(names_from = c(Country.Region, type), values_from = cumulative)

time_to_double = matrix(data = NA, nrow = (dim(df_states_wider)[1]-1), ncol=(dim(df_states_wider)[2]-1))
dim(time_to_double)

for (i in 1:ncol(time_to_double)){
  i = i+1
  for (j in 1:nrow(time_to_double)){
    k = j+1
    if (df_states_wider[k,i] == df_states_wider[j,i]){
      dt = NA
      time_to_double[j, (i-1)] = dt
    } else {
      dt = log(2)/log(as.numeric(df_states_wider[k,i]/df_states_wider[j,i]))
      time_to_double[j, (i-1)] = dt
    }
  }
}
rm(i, j, k, dt)
last_date = matrix(data = NA, nrow = 1, ncol=(dim(df_states_wider)[2]-1)) # for the last date, there is no doubling time calculable 
time_to_double = rbind(last_date, time_to_double)
rm(last_date)

dates =  df_states_wider%>%
  select(date)%>%
  as.vector()
time_to_double = cbind(dates, time_to_double)

colnames.df_states_wider = colnames(df_states_wider)
colnames(time_to_double) = colnames.df_states_wider
rm(colnames.df_states_wider, df_states_wider)

df_time_to_double = as.data.frame(time_to_double)

df_time_to_double = df_time_to_double%>%
  pivot_longer(-date, names_to = c("Country.Region", "type"), names_sep = "_", values_to = "dt")

df_time_to_double = df_time_to_double%>%
  mutate(growth.rate = (log(2)/dt)*100)
  
df_states = df_states%>%
  left_join(y = df_time_to_double, by = c("Country.Region", "type", "date"))
rm(df_time_to_double)

df_states = df_states%>%
  group_by(Country.Region, type)%>%
  mutate(growth.rate.mean = rollmean(growth.rate, 7,align = 'right',na.rm =  TRUE, fill = TRUE))%>%
  mutate(cases.mean = rollmean(cases, 7,align = 'right', na.rm = TRUE, fill = TRUE))


plot_growth_rate = df_states%>%
  filter(type == "confirmed")%>%
  filter(cumulative >= 100)%>%
  #filter(Continent == "Europe")%>%
  filter(Country.Region != "MS Zaandam" & Country.Region != "Diamond Princess")%>%
  filter(growth.rate != Inf)%>% # if no case gets recorded, the growth.rate == Inf
  filter(growth.rate >= 0)%>% # some confirmed events got corrected and given as neg. value
    ggplot(aes(x = date, y = growth.rate, group = Country.Region))+
    #ylim(0, 300)+
    geom_line(aes(color = Continent))+
    theme_bw()+
    labs(x = "Date", y = "day-to-day growth rate")+
    facet_grid(col = vars(Continent))
plot_growth_rate

plot_time_to_double = df_states%>%
  filter(type == "confirmed")%>%
  filter(cumulative >= 100)%>%
  #filter(Continent == "Europe")%>%
  filter(Country.Region != "MS Zaandam" & Country.Region != "Diamond Princess" & Country.Region != "China")%>%
  filter(dt != Inf)%>% # if no case gets recorded, the growth.rate == Inf
  filter(dt >= 0)%>% # some confirmed events got corrected and given as neg. value
    ggplot(aes(x = date, y = dt, group = Country.Region))+
    #ylim(0, 300)+
    geom_line(aes(color = Continent))+
    theme_bw()+
    labs(x = "Date", y = "day-to-day doubling time")+
    facet_grid(col = vars(Continent))
plot_time_to_double

plot_growth_rate_rollmean = df_states%>%
  filter(type == "confirmed")%>%
  filter(cumulative >= 100)%>%
  #filter(Continent == "Europe")%>%
  filter(Country.Region != "MS Zaandam" & Country.Region != "Diamond Princess")%>%
  filter(growth.rate != Inf)%>% # if no case gets recorded, the growth.rate == Inf
  filter(dt >= 0)%>% # some confirmed events got corrected and given as neg. value
  ggplot(aes(x = date, y = growth.rate.mean, group = Country.Region))+
  #ylim(0, 300)+
  geom_line(aes(color = Continent))+
  theme_bw()+
  labs(x = "Date", y = "7-Day rolling mean of growth rate")
  #facet_grid(col = vars(Continent))


  


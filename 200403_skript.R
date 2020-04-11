
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
  bind_rows(df_canada)

rm(df_provinces, df_colonies, df_canada) # tidy up

  # In df_states the lat and long for  is now missing.
  # data is obtained manualle from https://github.com/MISP/misp-dashboard/blob/master/data/country_code_lat_long.json

df_states$Lat[which(df_states$Country.Region == "Australia")] = -27.0000
df_states$Lat[which(df_states$Country.Region == "Australia")] = 133.0000

df_states$Lat[which(df_states$Country.Region == "Canada")] = 60.0000
df_states$Lat[which(df_states$Country.Region == "Canada")] = -95.0000

df_states$Lat[which(df_states$Country.Region == "China")] = 35.0000
df_states$Lat[which(df_states$Country.Region == "China")] = 105.0000


## Plot: Cummulative cases and types vs date for all countries
plot_cumulative_by_time_confirmed = df_states%>%
  group_by(Country.Region, type)%>%
  filter(type == "confirmed")%>%
    ggplot(aes(date, cumulative, fill = Country.Region))+
      geom_line()
plot_cumulative_by_time_death = df_states%>%
  group_by(Country.Region, type)%>%
  filter(type == "death")%>%
  ggplot(aes(date, cumulative, fill = Country.Region))+
  geom_line()
plot_cumulative_by_time_recovered = df_states%>%
  group_by(Country.Region, type)%>%
  filter(type == "recovered")%>%
  ggplot(aes(date, cumulative, fill = Country.Region))+
  geom_line()

## Plot: by continent


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
  filter(type == "confirmed")%>%
    ggplot()+
      geom_path(aes(days, cumulative, group = Country.Region), lineend = "round", color='grey50')+
      xlim(0, 50)+
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x)))+
      annotation_logticks(side="l") +
      labs(x =" Days since 100 confirmed cases", y ="Cumulative number of confirmed cases",
           titel = "Confirmed Cases") +
      theme_bw()+
    geom_line(data = df_doubling_time, aes(time_days, value, group = `Days to double`, color=`Days to double`),
              size = 1.5, linetype = 5)+
  scale_color_viridis_d()

plot_days_to_double_confirmed #warning message ist due to the restriction on days [0, x]

















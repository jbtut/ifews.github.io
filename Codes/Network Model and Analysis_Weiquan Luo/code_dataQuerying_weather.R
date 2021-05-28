setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list=ls())
dev.off()

library(tidyr)
library(rnoaa)
library(dplyr)
library(lubridate)
library(sf)
# stationUSA_info <- ghcnd_stations()
# save(stationUSA_info, file = "data/stationUSA_info.Rdata")
load("data/stationUSA_info.Rdata")
source("ud_function.R")

# subset the stationUSA_info by state, first_year, and last_year
station_info <- stationUSA_info %>% 
  filter(element %in% c("PRCP ", "TMAX", "TMIN")) %>% 
  filter(state %in% c("IA", "MO")) %>% 
  filter(first_year < "1980") %>% 
  filter(last_year >= "2019");  # head(station_info)

# pulling data from online: 
# list of avaliable variable from query: https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt
tictoc::tic()
monitors <- station_info$id
weather_data <- meteo_pull_monitors(monitors, var = c("prcp", "tmax", "tmin"),
                                    date_min = "1980-01-01",
                                    date_max = "2019-12-31") %>% 
  rename(Date = date, NOAA_ID = id)
tictoc::toc()

# remove the all() NA raw
rmRow <- weather_data %>% 
  select(prcp, tmax, tmin) %>% 
  is.na() %>%
  apply(MARGIN = 1, FUN = all)
weather_data <- weather_data[!rmRow,]


weather_data<- weather_data %>% left_join(station_info %>% 
                                 select(id, latitude,longitude) %>% 
                                 distinct, 
                               by = c("NOAA_ID" = "id")) %>% 
  select(NOAA_ID, longitude, latitude, Date, prcp, tmax, tmin); weather_data


prcp <- weather_data %>%
  select(NOAA_ID, longitude, latitude, Date, prcp) %>%
  setNames(c("id", "longitude", "latitude", "Date", "prcp")) %>% 
  mutate_at("prcp", function(x) x/10)

tmax <- weather_data %>%
  select(NOAA_ID, longitude, latitude, Date, tmax) %>%
  setNames(c("id", "longitude", "latitude", "Date", "tmax")) %>% 
  mutate_at("tmax", function(x) x/10) %>% 
  mutate(tmax = tmax + 273.15) %>% 
  filter(tmax > 0)

tmin <- weather_data %>%
  select(NOAA_ID, longitude, latitude, Date, tmin) %>%
  setNames(c("id", "longitude", "latitude", "Date", "tmin")) %>% 
  mutate_at("tmin", function(x) x/10) %>% 
  mutate(tmin = tmin + 273.15) %>% 
  filter(tmin > 0)

# save weather data 
save(weather_data,
     prcp, tmax, tmin,
     file = "data/weather_data.Rdata")
load("data/weather_data.Rdata", verbose = TRUE)



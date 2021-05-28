# this file is to gather the data information
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list=ls())
dev.off()

library(purrr)
library(dplyr)
library(ggplot2)
library(naniar)
library(tidyr)
library(lubridate)
library(stringr)

# data_area.RData
timeRange_data_area <- load("data/data_area.RData", verbose = TRUE) %>% 
  map(get) %>% 
  map(function(x){
    # test: waterQuality ->x
    x <- x[,-1] 
    x %>% 
      pivot_longer(-names(x)[1], names_to = "Variable",  values_to = "value") %>% 
      mutate(period = names(x)[1]) %>% 
      rename(date = names(x)[1]) %>% 
      na.omit() %>% 
      group_by(period, Variable) %>% 
      summarise(start_period = min(date), 
                end_period = max(date)) %>% 
      ungroup() %>% 
      select(Variable, period, start_period, end_period) %>% 
      mutate_at(c("start_period", "end_period"), as.character) %>% 
      separate(col = start_period, into = c("start_year", "start_month", "start_day")) %>% 
      separate(col = end_period, into = c("end_year", "end_month", "end_day")) %>% 
      mutate(start_period = ifelse(period == "day", paste(start_year, start_month, start_day, sep = "-"),
                                   ifelse(period == "month", paste(start_year, start_month, sep = "-"),
                                          ifelse(period == "year", paste(start_year), NA)))) %>% 
      mutate(end_period = ifelse(period == "day", paste(end_year, end_month, end_day, sep = "-"),
                                 ifelse(period == "month", paste(end_year, end_month, sep = "-"),
                                        ifelse(period == "year", paste(end_year), NA)))) %>% 
      mutate(timeRange = paste0(start_period, " to ", end_period)) %>% 
      select(Variable, period, timeRange)
  }) %>% 
  reduce(rbind) 
# water_data.Rdata
timeRange_data_water <- load("data/water_data.Rdata", verbose = TRUE) %>% 
  str_match(pattern = "wflow|gageHeight|twater") %>% 
  na.omit() %>% 
  map(get) %>% 
  map(function(x){
    # test: wflow ->x
    x <- x[,-c(1,2,3)] 
    x %>% 
      pivot_longer(-names(x)[1], names_to = "Variable",  values_to = "value") %>% 
      mutate(period = names(x)[1]) %>% 
      rename(date = names(x)[1]) %>% 
      na.omit() %>% 
      group_by(period, Variable) %>% 
      summarise(start_period = min(date), 
                end_period = max(date)) %>% 
      ungroup() %>% 
      select(Variable, period, start_period, end_period) %>% 
      mutate_at(c("start_period", "end_period"), as.character) %>% 
      mutate(timeRange = paste0(start_period, " to ", end_period)) %>% 
      select(Variable, period, timeRange)
  }) %>% 
  reduce(rbind) 
# weather_data.Rdata
timeRange_data_weather <- load("data/weather_data.Rdata", verbose = TRUE) %>% 
  str_match(pattern = "prcp|tmax|tmin") %>% 
  na.omit() %>% 
  map(get) %>% 
  map(function(x){
    # test: prcp ->x
    x <- x[,-c(1,2,3)] 
    x %>% 
      pivot_longer(-names(x)[1], names_to = "Variable",  values_to = "value") %>% 
      mutate(period = names(x)[1]) %>% 
      rename(date = names(x)[1]) %>% 
      na.omit() %>% 
      group_by(period, Variable) %>% 
      summarise(start_period = min(date), 
                end_period = max(date)) %>% 
      ungroup() %>% 
      select(Variable, period, start_period, end_period) %>% 
      mutate_at(c("start_period", "end_period"), as.character) %>% 
      mutate(timeRange = paste0(start_period, " to ", end_period)) %>% 
      select(Variable, period, timeRange)
  }) %>% 
  reduce(rbind) 
timeRange <- ls(pattern = "timeRange_") %>% 
  map(get) %>% 
  reduce(rbind) %>% 
  select(-period)

# combine dataSummary
dataSummary <- readr::read_csv("dataSummary.csv") %>% 
  select(-(`Time Range`)) %>% 
  left_join(timeRange, by = "Variable") %>% 
  rename("Time Range" = timeRange) %>% 
  select(c("Group", "Variable", "Description", "Unit",
           "Space Scale", "Time Scale", "Time Range",
           "Spatial downscaling", "Temporal downscaling",
           "Source", "Link"))
dataSummary %>% write.csv("dataSummary.csv", row.names = FALSE)




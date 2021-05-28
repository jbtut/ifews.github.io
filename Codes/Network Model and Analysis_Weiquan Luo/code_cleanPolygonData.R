# set wd
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list=ls())
dev.off()

library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd("data/raw excel data")

# import xlsx
file_names <- as.list(dir(pattern="*.xlsx"))
sheets_names <- lapply(file_names,readxl::excel_sheets)
# assign sheet name to df
for (i in 1:length(file_names)) {
  for (j in 1:length(sheets_names[[i]])) {
    assign(sheets_names[[i]][j], readxl::read_excel(file_names[[i]], sheet = sheets_names[[i]][j]))
  }
}; 
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# import csv
setwd("data/raw excel data"); dir();
temp = list.files(pattern="*.csv$")
tempname = sapply(strsplit(temp, split='.', fixed=TRUE), function(x) (x[1]))
# assign file name to df
for (i in 1:length(temp)) assign(tempname[i], readr::read_csv(temp[i]))
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv region_sf vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
# county_sf
county <- sf::st_as_sf(maps::map("county", plot = FALSE, fill = TRUE)) %>% 
  rename("geometry" = "geom")
county_sf <- subset(county, grepl("iowa,", county$ID)) %>%
  mutate(county = ID %>% stringr::str_replace("iowa,","")) %>%
  select(county, geometry) %>%
  mutate(county = tolower(county)) %>%
  mutate(county = gsub(x= county, " ","")) %>% 
  setNames(c("region", "geometry")) 

state_sf <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>% 
  rename("geometry" = "geom") %>% 
  select(ID, geometry) %>% 
  setNames(c("region", "geometry")) %>%  
  mutate(geometry = geometry %>% st_transform(crs = 2163)) %>% 
  mutate(geometry = geometry %>% lwgeom::st_snap_to_grid(size = 0.01) %>% 
           lwgeom::st_make_valid()) %>% 
  mutate(geometry = geometry %>% st_transform(crs = 4326)) %>% 
  st_cast("MULTIPOLYGON")

ia_sf <- state_sf %>% filter(region == "iowa")

# AgDistrict_sf
AgDistrict <- cornYield %>% filter(Year ==2010) %>% 
  select(County, `Ag District`) %>% 
  setNames(c("county", "AgDistrict")) %>% 
  distinct() %>% 
  arrange(county) %>% 
  mutate(county = gsub(x= county, " ","")) %>% 
  mutate(county = tolower(county)) %>% 
  right_join(county_sf, by =  c("county" = "region")) %>% 
  mutate(geometry = geometry %>% st_transform(crs = 2163)) %>% 
  mutate(geometry = geometry %>% lwgeom::st_snap_to_grid(size = 0.01) %>% 
           lwgeom::st_make_valid()) %>% 
  select(-county) %>% 
  mutate(geometry = geometry %>% st_transform(crs = 4326))
AgDistrict_sf <- st_sf(AgDistrict,
                       geometry = st_sfc(AgDistrict$geometry, 
                                         crs = 4326)) %>% 
  setNames(c("region", "geometry")) %>% 
  group_by(region) %>% 
  summarise(geometry = st_combine(x = geometry))%>%
  st_union(by_feature = TRUE) %>% 
  st_simplify(dTolerance = 1/1000)

regional_division_countyAgDistrictstate <- cornYield %>% 
  filter(Year ==2010) %>% 
  select(County, `Ag District`) %>% 
  setNames(c("county", "AgDistrict")) %>% 
  distinct() %>% 
  arrange(county) %>% 
  mutate(county = gsub(x= county, " ","")) %>% 
  mutate(county = tolower(county)) %>% 
  mutate(state = "iowa") %>% 
  mutate_all(as.factor)

county_sf <- county_sf %>% st_simplify()

save(AgDistrict_sf,
     county_sf,
     ia_sf,
     state_sf,
     regional_division_countyAgDistrictstate,
     file = "data/region_sf.RData")

load("data/region_sf.RData", verbose = TRUE)

# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ region_sf ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv polygon_data vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

# agriculture corn, soyben price
cornPrice <-  cornPrice %>% 
  rename(year = Year) %>% 
  pivot_longer(cols = -year, names_to = "month", values_to = "cornPrice") %>% 
  mutate(month = ifelse(month == "Sept", "Sep", month)) %>% 
  mutate(month = month %>% match(.,month.abb)) %>% 
  mutate(day = 1) %>% 
  mutate(period = lubridate::as_date(paste(year, month, day,sep="-"))) %>% 
  mutate(region = "iowa") %>% 
  select(region, period, cornPrice) %>% 
  rename(state = region, month = period)
soybeanPrice <- soybeanPrice %>% 
  rename(year = Year) %>% 
  pivot_longer(cols = -year, names_to = "month", values_to = "soybeanPrice") %>% 
  mutate(month = ifelse(month == "Sept", "Sep", month)) %>% 
  mutate(month = month %>% match(.,month.abb)) %>% 
  mutate(day = 1) %>% 
  mutate(period = lubridate::as_date(paste(year, month, day, sep="-"))) %>% 
  mutate(region = "iowa") %>% 
  select(region, period, soybeanPrice) %>% 
  rename(state = region, month = period)
  

# yield
cornYield <- cornYield %>%
  select(County, Year, Value) %>%
  setNames(c("county","year", "cornYield")) %>%
  arrange(county) %>%
  mutate(county = tolower(county)) %>%
  mutate(county = gsub(x= county, "\'","")) %>%
  mutate(county = gsub(x= county, " ","")) %>%
  mutate(year = lubridate::ymd(year, truncated = 2L))
cornSilage <- cornSilage %>%
  select("Ag District", Year, Value) %>%
  setNames(c("AgDistrict","year", "cornSilage")) %>%
  mutate(AgDistrict = AgDistrict %>% as.factor()) %>% 
  mutate(year = lubridate::ymd(year, truncated = 2L))
soybeanYield <- soybeanYield %>%
  select(County, Year, Value) %>%
  setNames(c("county","year", "soybeanYield")) %>%
  arrange(county) %>%
  mutate(county = tolower(county)) %>%
  mutate(county = gsub(x= county, "\'","")) %>%
  mutate(county = gsub(x= county, " ","")) %>%
  mutate(year = lubridate::ymd(year, truncated = 2L))


# this functionis for animal inventory
Period2Month <- function(Period){
  if (Period == "FIRST OF MAR") {
    Month = 3
  } else if (Period == "FIRST OF JUN") {
    Month = 6
  } else if (Period == "FIRST OF SEP") {
    Month = 9
  } else if (Period == "FIRST OF DEC") {
    Month = 12
  } else {
    Month = NULL
  }
  return(Month)
}


# USDA_NASS
chickenInventory <- chickenInventory %>%
  select(Year,Value, Period) %>% 
  mutate(Value = Value %>% as.numeric()) %>% 
  mutate(Year = Year %>% as.factor()) %>% 
  setNames(c("Year", "chickenInventory", "Period")) %>% 
  mutate(Month = purrr::map_dbl(Period, Period2Month) %>% as.character()) %>% 
  mutate(Year = as.character(Year)) %>% 
  mutate(Date = paste(Year, Month, sep = "-")) %>% 
  mutate(Date = lubridate::ymd(Date, truncated = 1L)) %>% 
  select(Date, chickenInventory) 
hogInventory <- hogInventory %>%
  select(Year,Value, Period) %>% 
  mutate(Value = Value %>% as.numeric()) %>% 
  mutate(Year = Year %>% as.factor()) %>% 
  setNames(c("Year", "hogInventory", "Period")) %>% 
  mutate(Month = purrr::map_dbl(Period, Period2Month) %>% as.character()) %>% 
  mutate(Year = as.character(Year)) %>% 
  mutate(Date = paste(Year, Month, sep = "-")) %>% 
  mutate(Date = lubridate::ymd(Date, truncated = 1L)) %>% 
  filter(Month == 12) %>% 
  select(Date, hogInventory) 
cattleInventory <- cattleInventory %>%
  filter(Value != "(D)") %>% 
  select(Year,County,Value, `Ag District`, `Ag District Code`) %>% 
  mutate(Value = Value %>% as.numeric()) %>% 
  mutate(Year = Year %>% as.factor()) %>% 
  setNames(c("Year","County","cattleInventory", "AgDistrict", "Code")) %>% 
  group_by(Year) %>% 
  summarise(cattleInventory = sum(cattleInventory), ncounty= n()) %>% 
  filter(ncounty == max(ncounty)) %>% ## remove 1977, because only 26 county reported
  mutate(Year = as.numeric(as.character(Year))) %>% 
  mutate(Date = lubridate::ymd(Year, truncated = 2L)) %>% 
  select(Date, cattleInventory)
animalInventory <- chickenInventory %>% 
  full_join(hogInventory) %>% 
  full_join(cattleInventory) %>% 
  pivot_longer(-Date, names_to = "animal", values_to = "value") %>% 
  mutate(year = lubridate::year(Date)) %>% 
  select(-Date) %>% 
  group_by(year, animal) %>% 
  summarise(value = mean(value, na.rm = TRUE)) %>% 
  pivot_wider(names_from= "animal", values_from = "value") %>% 
  ungroup %>% 
  mutate(state = "iowa") %>% 
  select(c("state", "year", "cattleInventory", "hogInventory")) %>% 
  mutate(year = lubridate::ymd(year, truncated = 2L)) %>% 
  na.omit()
hogInventory <- animalInventory %>% select(state, year, hogInventory)
cattleInventory <- animalInventory %>% select(state, year, cattleInventory)


eggProduction <- eggProduction %>% 
  rename(eggProduction = Value) %>% 
  select(Year, Period, State, eggProduction) %>% 
  mutate(Period = Period %>% stringr::str_to_title(string = ., locale = "en")) %>% 
  mutate(month = Period %>% match(.,month.abb)) %>% 
  mutate(day = 1) %>% 
  na.omit() %>% 
  mutate(period = lubridate::as_date(paste(Year, month, day,sep="-"))) %>% 
  mutate(region =  State %>% stringr::str_to_lower(string = ., locale = "en")) %>% 
  select(region, period, eggProduction) %>% 
  rename(state = region, month = period)

electricGeneration <- electricGeneration %>% 
  setNames(c("X1", "year", "month", "state", "X2", "source", "electricGeneration")) %>% 
  select(year, month, state, source, electricGeneration) %>% 
  filter(state == "IA") %>% 
  group_by(year, month, source) %>% 
  summarize(electricGeneration = sum(electricGeneration, na.rm = TRUE)) %>% 
  mutate(state = "iowa") %>% 
  ungroup() %>% 
  mutate(month = month %>% lubridate::ymd(truncated = 2)) %>% 
  select(state, month, electricGeneration)

electricitySale <- electricitySale %>% 
  setNames(c("Month", "electricitySale")) %>% 
  separate(col = Month,  c("month", "year")) %>% 
  mutate(month = month %>% match(., month.abb) ) %>% 
  mutate_at(c("month", "year"), as.numeric) %>% 
  mutate(year = ifelse(test = year > 50, yes = year + 1900, no = year + 2000)) %>% 
  mutate(month = lubridate::make_date(year, month)) %>% 
  mutate(state = "iowa") %>% 
  select(state, month, electricitySale)

renewablebiofuel <- renewablebiofuel %>% 
  rename(year = Year) %>% 
  mutate(state = "iowa") %>% 
  mutate(year = lubridate::ymd(year, truncated = 2L)) %>% 
  select(state, year, EtOHproduction, biodieselProduction) 
EtOHproduction <- renewablebiofuel %>% select(state, year, EtOHproduction)
biodieselProduction <- renewablebiofuel %>% select(state, year, biodieselProduction)

flood <- flood %>% 
  select(CZ_NAME_STR, BEGIN_DATE, EVENT_TYPE) %>% 
  setNames(c("region", "period", "stormEvent")) %>% 
  mutate(region = region %>% stringr::str_replace(" CO.", "")) %>% 
  mutate(region = region %>% stringr::str_replace(" \\(ZONE\\)", "")) %>% 
  mutate(region = region %>% stringr::str_to_lower(locale = "en")) %>% 
  mutate(stormEvent = "flood")
drought <- drought %>% 
  select(CZ_NAME_STR, BEGIN_DATE, EVENT_TYPE) %>% 
  setNames(c("region", "period", "stormEvent")) %>% 
  mutate(region = region %>% stringr::str_replace(" CO.", "")) %>% 
  mutate(region = region %>% stringr::str_replace(" \\(ZONE\\)", "")) %>% 
  mutate(region = region %>% stringr::str_to_lower(locale = "en")) %>% 
  mutate(stormEvent = "drought")
highWind <- highWind %>%  
  select(CZ_NAME_STR, BEGIN_DATE, EVENT_TYPE) %>% 
  setNames(c("region", "period", "stormEvent")) %>% 
  mutate(region = region %>% stringr::str_replace(" CO.", "")) %>% 
  mutate(region = region %>% stringr::str_replace(" \\(ZONE\\)", "")) %>% 
  mutate(region = region %>% stringr::str_to_lower(locale = "en")) %>% 
  mutate(stormEvent = "highWind")
stormEvent <- flood %>% 
  rbind(drought) %>% 
  rbind(highWind) %>% # head(1000) %>% 
  separate(period, c("month", "day", "year"), "/") %>% 
  mutate_at(c("month", "day", "year"), as.numeric) %>% 
  mutate(year = ifelse(test = year > 50, yes = year + 1900, no = year + 2000)) %>% 
  mutate(period = lubridate::make_date(year, month, day)) %>% 
  select(region, period, stormEvent) %>% 
  rename(county = region, day = period) %>% 
  na.omit() %>% 
  distinct() %>% 
  mutate(value =1) %>% 
  pivot_wider(names_from = stormEvent, values_from = value, values_fill = list(value = 0)) %>% 
  tibble::rowid_to_column() %>% 
  mutate(year = lubridate::year(day), 
         month = lubridate::month(day)) %>% 
  mutate(month = paste(year, month, sep = "-")) %>% 
  select(county, month, flood, drought, highWind) %>% 
  group_by(county, month) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  mutate(month = month %>% ymd(truncated = 1L)) %>% 
  merge(expand.grid(month = seq(min(.$month), max(.$month), by = "1 month"), 
                    county = .$county %>% unique), 
        ., by = c("month", "county"),  all.x = TRUE) %>% 
  mutate(year = year(month)) %>% 
  complete(year, nesting(month,county), fill = list(flood = 0, drought = 0, highWind = 0)) %>% 
  select(county, month, flood, drought, highWind)
flood <- stormEvent %>% select(county, month, flood)
drought <- stormEvent %>% select(county, month, drought)
highWind <- stormEvent %>% select(county, month, highWind)

waterQuality <- waterQuality %>% 
  filter(analyte %in% c("Escherichia coli", "Dissolved oxygen (DO)", 
                        "Turbidity", "Microcystin")) %>% 
  select(county, sampleDate, analyte, result) %>% 
  tidyr::separate(col = sampleDate, into = c("month", "day", "year"), convert = TRUE) %>% 
  mutate(year = ifelse(test = year > 50, yes = year + 1900, no = year + 2000)) %>% 
  mutate(day = paste(year, month, day, sep = "-") %>% lubridate::ymd()) %>% 
  select(county, day, analyte, result) %>% 
  setNames(c("county", "day", "analyte", "waterQuality")) %>% 
  mutate_if(is.character, as.factor) %>% 
  group_by(county, day, analyte) %>% 
  summarise(waterQuality = waterQuality %>% mean(na.rm = TRUE)) %>% 
  ungroup() %>%
  tidyr::pivot_wider(names_from = analyte, values_from = waterQuality, 
                     values_fill = list(waterQuality = NA), 
                     values_fn = list(waterQuality = mean)) %>% 
  setNames(c("county", "day", "Ecoli", "dissolvedOxygen", "microcystin", "turbidity")) %>% 
  mutate(county = county %>% stringr::str_to_lower(locale = "en")) 
Ecoli <- waterQuality %>% select(county, day, Ecoli)
dissolvedOxygen <- waterQuality %>% select(county, day, dissolvedOxygen)
microcystin <- waterQuality %>% select(county, day, microcystin)
turbidity <- waterQuality %>% select(county, day, turbidity)

fishKill <- fishKill %>% 
  select(date, county) %>% 
  mutate(fishKill = 1) %>% 
  mutate(date = date %>% as.character() %>% stringr::str_sub(end = 10) %>% ymd(truncated = 1L)) %>% 
  rename(day = date) %>% 
  select(county, day, fishKill) %>% 
  mutate(year = lubridate::year(day), 
         month = lubridate::month(day)) %>% 
  mutate(month = paste(year, month, sep = "-")) %>% 
  select(county, month, fishKill) %>% 
  mutate(county = county %>% stringr::str_to_lower(locale = "en")) %>% 
  group_by(county, month) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  mutate(month = month %>% ymd(truncated = 1L)) %>% 
  merge(expand.grid(month = seq(min(.$month), max(.$month), by = "1 month"), 
                    county = .$county %>% unique), 
        ., by = c("month", "county"),  all.x = TRUE) %>% 
  mutate(year = year(month)) %>% 
  complete(year, nesting(month,county), fill = list(fishKill = 0)) %>% 
  select(county, month, fishKill)

electricityPrice <- electricityPrice %>% 
  janitor::row_to_names(row_number = 1) %>% 
  mutate_if(is.numeric, signif, digits = 1 ) %>% 
  readr::type_convert() %>% 
  filter(State == "IA") %>% 
  mutate(state = "iowa") %>% 
  select(state, Year, Total) %>% 
  mutate(Year = Year %>% lubridate::ymd(truncated = 2L)) %>% 
  setNames(c("state", "year", "electricityPrice"))

govExpenditure <- govExpenditure %>% 
  select(`Budget FY`, `Fiscal Period`, `Amount`) %>% 
  setNames(c("year", "month", "govExpenditure")) %>% 
  mutate(month = lubridate::ymd(paste(year, month, sep = "-"),  truncated = 1L)) %>% 
  group_by(month) %>% 
  summarise(govExpenditure = govExpenditure %>% sum(na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(state = "iowa") %>% 
  select(state, month, govExpenditure) %>% 
  na.omit()
govRevenue <- govRevenue %>% 
  select(`Budget FY`, `Fiscal Period`, `Amount`) %>% 
  setNames(c("year", "month", "govRevenue")) %>% 
  mutate(month = lubridate::ymd(paste(year, month, sep = "-"),  truncated = 1L)) %>% 
  group_by(month) %>% 
  summarise(govRevenue = govRevenue %>% sum(na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(state = "iowa") %>% 
  select(state, month, govRevenue) %>% 
  na.omit()

employment <- employment %>% 
  select(Year, Quarter, `Area Name`, `Month 1`, `Month 2`, `Month 3`) %>% 
  setNames(names(.) %>% stringr::str_remove(pattern = " ")) %>% 
  rename(county = AreaName) %>% 
  filter(county != "Statewide") %>% 
  mutate(county = county %>% stringr::str_to_lower(locale = "en")) %>% 
  group_by(Year, Quarter, county) %>% 
  summarise(Month1 = sum(Month1, na.rm = TRUE),
            Month2 = sum(Month2, na.rm = TRUE),
            Month3 = sum(Month3, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_longer(-c(Year, Quarter, county), names_to = "month",  values_to = "employment") %>% 
  mutate(month = month %>% stringr::str_remove(pattern = "Month") %>% as.integer()) %>% 
  mutate(month = month * Quarter) %>% 
  mutate(month = lubridate::ymd(paste(Year, month, sep = "-"), truncated = 1L)) %>% 
  select(county, month, employment) %>% 
  arrange(month, county)


dieselPrice <- dieselPrice %>%
  janitor::row_to_names(row_number = 1) %>% 
  select(Date, starts_with("Midwest")) %>% 
  readr::type_convert() %>% 
  setNames(c("month", "dieselPrice")) %>% 
  na.omit() %>% 
  separate(col = month, into = c("month", "year")) %>% 
  mutate(month = month %>% match(.,month.abb)) %>% 
  mutate(month = paste(year, month, sep = "-") %>% lubridate::ymd(truncated = 1L)) %>% 
  mutate(state = "iowa") %>% 
  select(state, month, dieselPrice)

# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ polygon_data ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

period_by <- c("day", "month", "year")
region_by <- c("county_sf", "AgDistrict_sf", "state_sf")

save(eggProduction, # state month # USDA NASS: https://www.nass.usda.gov/
     cornYield, # county  year # USDA NASS: https://www.nass.usda.gov/
     soybeanYield, # county  year # USDA NASS: https://www.nass.usda.gov/
     cornSilage, # AgDistrict year # USDA NASS: https://www.nass.usda.gov/
     cattleInventory, hogInventory, # year state # USDA NASS: https://www.nass.usda.gov/
     cornPrice, soybeanPrice, # state month # ISU Extension and Outreach, Ag Decision Maker	https://www.extension.iastate.edu/agdm/crops/html/a2-11.html
     flood, drought, highWind, # county month # NOAA Storm Events Database: https://www.ncdc.noaa.gov/stormevents/
     fishKill, # county month # Iowa DNR Fish Kill Database: https://programs.iowadnr.gov/fishkill/
     dieselPrice, # state month #EIA Gasoline and Diesel Fuel Update: https://www.eia.gov/petroleum/gasdiesel/
     electricGeneration, # state month #  State-level generation and fuel consumption data (EIA-923): https://www.eia.gov/electricity/data.php
     electricitySale, # state month # Monthly Form EIA-861M (formerly EIA-826) detailed data: https://www.eia.gov/electricity/data.php#sales: https://www.eia.gov/electricity/data.php#sales
     electricityPrice, # state year # Annual retail price (EIA-861): https://www.eia.gov/electricity/data.php#sales
     EtOHproduction, biodieselProduction, # state year # Iowa Renewable Fuels Association: https://iowarfa.org/resource-center/statistics/
     employment, # county month # Iowa Workforce Development, QCEW: https://www.iowaworkforcedevelopment.gov/quarterly-census-employment-and-wages
     govRevenue, # state month $ State of Iowa's data portal: https://data.iowa.gov/State-Government-Finance/State-of-Iowa-Revenue/urps-v5ck
     govExpenditure, # state month # State of Iowa's data portal: https://data.iowa.gov/State-Government-Finance/State-of-Iowa-Expenditures/mn9y-cwp6
     Ecoli, dissolvedOxygen, microcystin, turbidity, # county day # AQuIA, Iowa DNR Surface Water Monitoring data: https://programs.iowadnr.gov/aquia/
     file = "data/polygon_data.RData")

load("data/polygon_data.RData", verbose = TRUE)


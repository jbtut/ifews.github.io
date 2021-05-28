setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list=ls())
dev.off()

library(dataRetrieval)
library(lubridate)
library(dplyr)
library(purrr)
library(furrr)
library(tidyr)
library(sf)
source("ud_function.r")

# query site information for state around IA
states <- c("NE", "IA", "KS", "MO", "IL", "SD", "MN", "WI")
siteMidWest_info <- read_siteMidWest_info(states = states, update =FALSE)

# rbind site information only for select parm_cd
siteMidWest_parm <- rbind(siteMidWest_info %>% 
                            filter(end_date > "2019-10-31") %>% 
                            filter(data_type_cd == "dv") %>% 
                            filter(parm_cd %in% c("00060", "00010", "99133", "00065")) %>% 
                            filter(stat_cd == "00003"), 
                          siteMidWest_info %>% 
                            filter(end_date > "2019-12-31") %>% 
                            filter(data_type_cd == "qw") %>%
                            filter(parm_cd %in% "80154"))

# convert site dataframe to sf
siteMidWest_sf <- site2sf(df=siteMidWest_parm,
                          id_cn="site_no", 
                          Lon_cn="dec_long_va",
                          Lat_cn="dec_lat_va",
                          crs= 4326)

buffer_dist <- 10000

# import HUC 07 10, buffering
HU2_bf <- st_union(sf::st_read("data/HUC_IA/WBD_07_HU2_Shape/WBDHU2.shp", quiet = F), 
                   sf::st_read("data/HUC_IA/WBD_10_HU2_Shape/WBDHU2.shp", quiet = F)) %>% 
  thinshp() %>% 
  st_transform(crs = 2163) %>% 
  st_buffer(dist = buffer_dist) %>% 
  st_transform(crs = 4326)

# import boundary, convert sf, buffering
IAMO_sf_bf <- sf::st_as_sf(maps::map("state", 
                                     plot = FALSE, 
                                     fill = TRUE)) %>%
  # filter(ID %in% c("iowa","missouri")) %>% 
  filter(ID %in% states) %>% 
  st_transform(crs = 2163) %>% 
  st_buffer(dist = buffer_dist) %>%
  st_transform(crs = 4326)

# intersection between site and boundary with buffering
siteMidWest_select_sf <- siteMidWest_sf %>% 
  #st_intersection(IAMO_sf_bf) %>% 
  st_intersection(HU2_bf)

ggplot() +
  #geom_sf(data = IAMO_sf_bf) +
  geom_sf(data = HU2_bf) +
  geom_sf(data = siteMidWest_select_sf)

siteMidWest_select <- siteMidWest_parm %>% 
  filter(site_no %in% siteMidWest_select_sf$id); siteMidWest_select

# remove unnessary datastream
siteMidWest_select <- siteMidWest_select %>% 
  filter(site_no !="05420500" | parm_cd != "00010" | ts_id !=42791) %>% 
  filter(site_no !="05420500" | parm_cd != "99133" | ts_id !=230012) %>% 
  filter(site_no !="05544385" | parm_cd != "00065" | ts_id !=155322) %>% 
  filter(site_no !="06485950" | parm_cd != "00065" | ts_id !=247531) %>% 
  filter(site_no !="06903900" | parm_cd != "00060" | ts_id !=43342) %>% 
  filter(site_no !="424848088083100" | parm_cd != "00065" | ts_id !=155613) 
  
siteMidWest_select <- siteMidWest_select %>% 
  filter(site_no !="411219096010601") %>% 
  filter(site_no !="05536995") %>% 
  filter(site_no !="05536137") %>% 
  filter(site_no !="05340500") %>% 
  filter(site_no !="05357206") %>% 
  filter(site_no !="05398000") %>% 
  filter(site_no !="473423095053301") %>% 
  filter(site_no !="054279465") 

siteMidWest_select <- siteMidWest_select %>% filter(!is.na(stat_cd))

# check for parm_cd
siteMidWest_select %>% group_by(stat_cd) %>% summarise(n=n()) %>% ungroup() %>% arrange((desc(n)))
# check for parm_cd
siteMidWest_select %>% group_by(parm_cd) %>% summarise(n=n())  %>% ungroup() %>% arrange((desc(n)))
# check duplicate sit_no in data stream  
siteMidWest_select %>% group_by(site_no) %>% summarise(n=n()) %>% ungroup() %>%  arrange((desc(n)))
# check for each lake each parm_cd
siteMidWest_select %>% group_by(site_no, parm_cd) %>% summarise(n=n()) %>% ungroup() %>% arrange((desc(n)))
# check a specific site
# a <- siteMidWest_select %>% filter(site_no == "424848088083100");View(a)
# a <- siteMidWest_select %>% filter(parm_cd == "00010") ;View(a)

# save selected site info
save(siteMidWest_select,
     file = "data/siteMidWest_select.Rdata")
load("data/siteMidWest_select.Rdata", verbose = TRUE)

# pulling water quality data from online: 
library(tictoc)
tic()
pCodes <- siteMidWest_select$parm_cd %>% unique(); pCodes
water_data <- plyr::llply(pCodes, function(pCodes) {
  querydatas(pCode= pCodes,
             site = siteMidWest_select,
             startDate = "1980-01-01",
             endDate = "2019-12-31") %>% 
    left_join(siteMidWest_select %>% # add long lat
                select(site_no, dec_long_va, dec_lat_va) %>% 
                distinct(), 
              by = "site_no") %>% 
    select(site_no, dec_long_va, dec_lat_va, parm_cd, data_type_cd, stat_cd, data) %>% 
    rename(longitude = dec_long_va, latitude = dec_lat_va) %>% 
    mutate(isnull = map_dbl(data, is_null)) %>%  # remove null stie
    filter(isnull==0) %>% 
    select(-isnull) %>% 
    mutate(site_no = paste0("USGS", site_no))
}); water_data
toc()


# parameterCdFile %>% filter(parameter_cd== "80154")
# 00010: Temperature, water, degrees Celsius
# 00060: Stream flow (Discharge), mean. daily, cubic feet per second
# 99133: Inorganic nitrogen (nitrate and nitrite) in situ, milligrams per liter as nitrogen
# 80154: Suspended sediment concentration, milligrams per liter
# 00065: Gage height, feet

# 00060: charge, cubic feet per second, Stream flow, mean. daily
wflow <- water_data[[1]] %>%
  unnest(cols = c(data)) %>%
  select(site_no, longitude, latitude, sample_dt, result) %>%
  setNames(c("id", "longitude", "latitude", "Date", "wflow")) %>% 
  mutate(wflow = purrr::map_dbl(wflow, function(x) if_else(x < 0, 0, x)))

# 00065: Gage height, feet
gageHeight <- water_data[[2]] %>%
  unnest(cols = c(data)) %>%
  select(site_no, longitude, latitude, sample_dt, result) %>%
  setNames(c("id", "longitude", "latitude", "Date", "gageHeight"))

# 00010: Temperature, water, degrees Celsius, Temperature, water
twater<- water_data[[3]] %>%
  unnest(cols = c(data)) %>%
  select(site_no, longitude, latitude, sample_dt, result) %>%
  setNames(c("id", "longitude", "latitude", "Date", "twater")) %>% 
  mutate(twater = twater + 273.15) %>% 
  filter(twater > 0)

# 99133: Nitrate plus nitrite, water, in situ, milligrams per liter as nitrogen
nitrateNitrite <- water_data[[4]] %>%
  unnest(cols = c(data)) %>%
  select(site_no, longitude, latitude, sample_dt, result) %>%
  setNames(c("id", "longitude", "latitude", "Date", "nitrateNitrite"))

# 80154: Suspended sediment concentration (SSC), milligrams per liter
SSC <- water_data[[5]] %>%
  unnest(cols = c(data)) %>%
  select(site_no, longitude, latitude, sample_dt, result) %>%
  setNames(c("id", "longitude", "latitude", "Date", "SSC"))


save(water_data, 
     wflow, gageHeight, twater, nitrateNitrite,
     file = "data/water_data.Rdata")
load("data/water_data.Rdata", verbose = TRUE)


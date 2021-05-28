setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list=ls())
dev.off()
options(future.globals.maxSize = 600*1024^2) # 600 Mb

# load pkg
## data modification
library(tibble)
library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)
library(sf)
library(stringr)
library(forcats)
## utility
library(purrr)
library(furrr)
library(tictoc)
## visuliation
library(ggplot2)
library(gRain)
library(Rgraphviz)
library(igraph)
# modeling
library(bnlearn)
library(XMRF)

# load function
source("ud_function.R")

# load data
load("data/region_sf.RData", verbose = TRUE)
load("data/water_data.Rdata", verbose = TRUE)
load("data/weather_data.Rdata", verbose = TRUE)
load("data/polygon_data.RData", verbose = TRUE)
rm(weather_data, water_data)
dataSummary <- readr::read_csv("dataSummary.csv") %>% 
  select("Variable", "Space Scale", "Time Scale", "Spatial downscaling", "Temporal downscaling") %>% 
  setNames(c("var", "orig_region", "orig_period", "Spatialdownscaling", "Temporaldownscaling"))

# set control boundary
boundary <- "ia_sf"

# crop point data by the base boundary 
point_data <- c("prcp", "tmax", "tmin", "twater", "wflow", "gageHeight")
for (i in 1:length(point_data)) assign(point_data[i], point_data[i] %>% 
                                         get() %>%
                                         crop_data_by_boundary(boundary = boundary))
# rescale
for (i in 1:length(point_data)) assign(point_data[i], point_data[i] %>% 
                                         get() %>%
                                         rescale_point_data(x= ., to = c(1, 5)))

# crop the all geo-boundary by the base boundary
regions <- c("county_sf", "AgDistrict_sf")
for (i in 1:length(regions)) assign(regions[i], crop_region_by_boundary(region_sf = regions[i],
                                                                        boundary = boundary))
periods <- c("day", "month", "year")
regions <- c("county_sf", "AgDistrict_sf", "state_sf")
# Visialize regional division
ggplot() +
  geom_sf(data = county_sf, aes(fill = region), alpha = 0, size =0.5, color = "black") +
  geom_sf(data = AgDistrict_sf, aes(fill = region), alpha = 0, size = 1, color = "black") +
  geom_sf(data = ia_sf, aes(fill = region), alpha = 0, size = 1, color = "black") +
  theme(legend.position = "none") +
  ggtitle("Regional division of Iowa by Agriculture Distrist and county")

# downscale spatiotemporal data by variables
## point_data
initialDataSummary <- dataSummary %>% 
  filter(orig_region == "point") %>% 
  filter(var != "twater")
### initial joined dataset
initial_pointData_downscaling(point_datas = initialDataSummary$var, 
                              periods = periods, 
                              regions = regions, 
                              Spatial_downscalings = initialDataSummary$Spatialdownscaling, 
                              Temporal_downscalings = initialDataSummary$Temporaldownscaling)
### other point data
pointdataSummary <- dataSummary %>% 
  filter(var == "twater")
# rm(list = ls(pattern = "pointdata_"))
point_data_downscaling(point_datas = pointdataSummary$var, 
                       periods = periods, 
                       regions = regions, 
                       Spatial_downscalings = pointdataSummary$Spatialdownscaling,
                       Temporal_downscalings = pointdataSummary$Temporaldownscaling)

## polygon_data
### construcutre original scale and find the avaiable downscale, save as a dict
period_division <- tibble() %>% 
  rbind(tibble(from = "year", to = "month")) %>% 
  rbind(tibble(from = "year", to = "day")) %>% 
  rbind(tibble(from = "month", to = "day")) 
region_division <- tibble() %>% 
  rbind(tibble(from = "AgDistrict", to = "county")) %>% 
  rbind(tibble(from = "state", to = "county")) %>% 
  rbind(tibble(from = "state", to = "AgDistrict")) 
polygon_data_dict <- dataSummary %>% 
  filter(orig_region != "point") %>% 
  mutate(dest_region = orig_region %>% purrr::map(function(division_by){
    region_division %>% 
      filter(to == division_by) %>% 
      select(from) %>% 
      rbind(division_by) %>% 
      unlist %>% paste() 
  })) %>% 
  unnest(dest_region) %>% 
  mutate(dest_period = orig_period %>% purrr::map(function(division_by){
    period_division %>% 
      filter(to == division_by) %>% 
      select(from) %>% 
      rbind(division_by) %>% 
      unlist %>% paste() 
  })) %>% 
  unnest(dest_period) %>% 
  mutate_all(as.character) %>% 
  mutate(identical_scale = ifelse(orig_period == dest_period & orig_region == dest_region, TRUE, FALSE))
polygon_data_downscaling(polygon_data_dict = polygon_data_dict, 
                         regional_division_df = regional_division_countyAgDistrictstate)


## modeling at multiscale
### create joined data and model
# test: period <- periods[2]; region <- regions[3]
rm(list = ls(pattern = "joined|model"))
for (period in periods){
  for (region in regions){
    set.seed((10))
    scale_label <- paste0(stringr::str_sub(region, end = -4L), "X", period)
    
    # create joined area and point data
    cat("\n\n"); cat(paste0("joined_", scale_label))
    tmp_joined <- forwardSelection(scale_label, rrp = 0.8) %>% 
      mutate(period = as.factor(period)) %>% 
      mutate(region = as.factor(region)) %>% 
      select(region, period, everything()) %>% 
      arrange(region, period) %>% 
      # mutate_if(is.numeric, function(x) log2(x+1)) %>% # log transform
      # mutate_if(is.numeric, scales::rescale, to = c(1, 5)) # %>% # rescaling
      mutate_at(c("region", "period"), as.character) %>% 
      mutate_if(is.numeric, arules::discretize, method = "interval", breaks = 5) %>%  # discretize
      mutate_if(is.factor, as.numeric) # discretize # %>% add_time_space_varible(scale_label = scale_label)
      
    # tmp_joined %>% summary() # check
    
    # save joined
    tmp_joined %>% 
      assign(value = ., 
             x =  paste0("joined", "_", scale_label),
             envir = .GlobalEnv)
    
    # rm na
    tmp_joined <- tmp_joined %>% 
      arrange(region, period) %>% 
      na.omit() 
    cat(paste0(" (Dim: ", nrow(tmp_joined), " ", ncol(tmp_joined), ")"))
    
    # create model and strength
    if (nrow(tmp_joined)!=0){
      cat("\n"); cat("model ")
      tmp_model <- tmp_joined %>%
        xmrfModeling(.)
      tmp_model %>% 
        assign(value = ., x = paste0("model", "_", scale_label), envir = .GlobalEnv)
    }
  }
}
ls(pattern = "pointdata_|joined_|model_|strength_")
save(list = ls(pattern = "pointdata_|joined_|model_|strength_"),
     file = "data/join_model_strength.Rdata")
# load("data/join_model_strength.Rdata", verbose = TRUE)

# visualization
dev.off()
# test: period <- periods[2]; region <- regions[3]
par(mfrow=c(3,3), mar=rep(1,4)) ## plot all 9 submodels
for (period in periods){
  for (region in regions){
    set.seed((15))
    scale_label <- paste0(stringr::str_sub(region, end = -4L), "X", period)
    plotMatrix_xmrf(scale_label)
  }
}

# Summarise Networks
networkSummary <- ls(pattern = "model_", envir = .GlobalEnv) %>% 
  stringr::str_sub(start=7) %>%
  tibble(Scale = .) %>% 
  mutate(networkSummary = Scale %>% map(SummariseNetwork)) %>% 
  unnest(networkSummary) %>% 
  separate(col = Scale, into = c("Space scale", "Time scale"), sep = "X" ) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(`Space scale` = fct_relevel(`Space scale`, "county", "AgDistrict", "state")) %>% 
  mutate(`Time scale` = fct_relevel(`Time scale`, "day", "month", "year")) ; networkSummary 
write.csv(networkSummary %>% mutate_if(is.numeric, signif, digits = 3), file = "networkSummary.csv")
networkSummary %>% 
  psych::pairs.panels(., stars=TRUE, density = FALSE, ellipses=FALSE)


ggpubr::ggarrange(
  ggpubr::ggarrange(combineEdge(by_scale = "spaceScale") + xlab(NULL) + ylab(NULL),
                    combineEdge(by_scale = "timeScale") + xlab(NULL) + ylab(NULL) ,
                    ncol = 2, nrow = 1),
  combineEdge(by_scale = "none") + xlab(NULL) + ylab(NULL),
  ncol = 1, nrow = 2)


ggpubr::ggarrange(ggpubr::ggarrange(networkSummary %>% 
                                      ggplot(aes(x= `Time scale`, y = `# of Variable`)) +
                                      geom_point(aes(shape = `Space scale`)) +
                                      geom_line(aes(group = `Space scale`)) +
                                      ylab("# of selected variables") +
                                      xlab(NULL) +
                                      theme_bw(),
                                    networkSummary %>% 
                                      ggplot(aes(x= `Time scale`, y = `# of Data point`)) +
                                      geom_point(aes(shape = `Space scale`)) +
                                      geom_line(aes(group = `Space scale`)) +
                                      ylab("# of datapoint") +
                                      xlab("Time Scale") +
                                      theme_bw() +
                                      theme(legend.position = c(.91, 0.67)),
                                    labels = NA, common.legend = TRUE, legend = "top",
                                    ncol = 1, nrow = 2),
                  ggpubr::ggarrange(networkSummary %>% 
                                      ggplot(aes(x= `Space scale`, y = `# of Variable`)) +
                                      geom_point(aes(shape = `Time scale`)) +
                                      geom_line(aes(group = `Time scale`)) +
                                      ylab(NULL) +
                                      xlab(NULL) +
                                      theme_bw(),
                                    networkSummary %>% 
                                      ggplot(aes(x= `Space scale`, y = `# of Data point`)) +
                                      geom_point(aes(shape = `Time scale`)) +
                                      geom_line(aes(group = `Time scale`)) +
                                      ylab(NULL) +
                                      xlab("Space Scale") +
                                      theme_bw() +
                                      theme(legend.position = c(.91, 0.67)),
                                    labels = NA, common.legend = TRUE, legend = "top",
                                    ncol = 1, nrow = 2),
                  labels = "AUTO", ncol = 2, nrow = 1)

# degree Summary by group
ls(pattern = "model_", envir = .GlobalEnv) %>% 
  stringr::str_sub(start=7) %>%
  map(degreeNetork) %>% 
  reduce(rbind) %>% 
  separate(col = Scale, into = c("Space scale", "Time scale"), sep = "X" ) %>%
  mutate(`Space scale` = fct_relevel(`Space scale`, "county", "AgDistrict", "state")) %>% 
  mutate(`Time scale` = fct_relevel(`Time scale`, "day", "month", "year")) %>% 
  ggplot(aes(x = Degree)) +
  geom_bar() +
  facet_grid(`Time scale` ~ `Space scale`) 

# betweenness Summary by group
ls(pattern = "model_", envir = .GlobalEnv) %>% 
  stringr::str_sub(start=7) %>%
  map(betweennessNetork) %>% 
  reduce(rbind) %>% 
  separate(col = Scale, into = c("Space scale", "Time scale"), sep = "X" ) %>% 
  mutate(`Space scale` = fct_relevel(`Space scale`, "county", "AgDistrict", "state")) %>% 
  mutate(`Time scale` = fct_relevel(`Time scale`, "day", "month", "year")) %>% 
  mutate(`Group` = fct_relevel(`Group`, "Food", "Water", "Weather", "Energy", "Economic","Event")) %>% 
  ggplot() +
  geom_boxplot(aes(x= Group,
                   y = `Betweenness`)) +
  facet_grid(`Time scale` ~ `Space scale`) +
  theme_bw() +
  xlab(NULL)+
  theme(axis.text.x = element_text(angle = -30))


ggpubr::ggarrange(networkSummary %>% 
                    ggplot(aes(x= `Time scale`, y = `Assortativity Coefficient`)) +
                    geom_point(aes(shape = `Space scale`)) +
                    geom_line(aes(group = `Space scale`)) +
                    ylab(NULL) +
                    xlab(NULL) +
                    theme_bw(),
                  networkSummary %>% 
                    ggplot(aes(x= `Space scale`, y = `Assortativity Coefficient`)) +
                    geom_point(aes(shape = `Time scale`)) +
                    geom_line(aes(group = `Time scale`)) +
                    ylab(NULL) +
                    xlab(NULL) +
                    theme_bw(),
                  labels = NA, common.legend = FALSE, legend = "top",
                  ncol = 2, nrow = 1)

### normalizty test for
apply(joined_countyXdate %>% select_if(is.numeric), 2, shapiro.test)
apply(joined_countyXdate %>% select_if(is.numeric), 2, nortest::ad.test)

### correlation, density at each scale
#  %>% mutate_if(is.numeric, log2)
psych::pairs.panels(joined_countyXday, stars=TRUE)
psych::pairs.panels(joined_AgDistrictXday, stars=TRUE)
psych::pairs.panels(joined_stateXday, stars=TRUE)
psych::pairs.panels(joined_countyXmonth, stars=TRUE)
psych::pairs.panels(joined_AgDistrictXmonth, stars=TRUE)
psych::pairs.panels(joined_stateXmonth, stars=TRUE)
psych::pairs.panels(joined_countyXyear, stars=TRUE)
psych::pairs.panels(joined_AgDistrictXyear, stars=TRUE)
psych::pairs.panels(joined_stateXyear, stars=TRUE)

joined_countyXday %>% .[,-c(1,2)] %>% as.matrix() %>% psych::pairs.panels(stars=TRUE)




# Regional division of Iowa by Agriculture Distrist and county
var_sf <- point_data %>% 
  purrr::map(function(x){
    get(x =x, envir = .GlobalEnv) %>% 
      site2sf() %>% 
      select(geometry) %>% 
      unique() %>% 
      mutate(variable = x)}
  ) %>% 
  purrr::reduce(rbind) %>% 
  select(variable, geometry) 
nvar_sf <- var_sf %>%  
  sf::st_intersection(county_sf) %>% 
  sf::st_drop_geometry() %>% 
  group_by(region) %>% 
  distinct() %>% 
  summarise(nvar = n()) %>% 
  right_join(county_sf, by = "region") %>% 
  mutate(nvar = as.factor(nvar)) %>% 
  st_as_sf()
nvar_sf %>% 
  ggplot() +
  geom_sf(aes(fill = nvar), size =0.5, color = "black") +
  scale_fill_brewer(palette = "Blues") +
  geom_sf(data = st_union(AgDistrict_sf, by_feature = TRUE), alpha = 0, size = 1, color = "black") +
  geom_sf(data = var_sf, alpha = 1/6) +
  theme(panel.background = element_blank()) 
nvar_sf %>% st_drop_geometry() %>% filter(nvar == 6)

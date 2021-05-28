
# vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv  query water   vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv


# query site information for state around IA
read_siteMidWest_info <- function(states, update){
  if (update ==TRUE){
    siteMidWest_info <- plyr::mdply(states, function(x) {
      site_info_new  <- readNWISdata(stateCd = x, service="site", seriesCatalogOutput=TRUE,
                                     startDate = "2000-01-01",
                                     endDate = "2018-12-31")}) %>% 
      select(-X1) %>% 
      as_tibble() %>% 
      mutate_at(.vars = c("begin_date", "end_date"), ymd)
    saveRDS(siteMidWest_info, file = "data/siteMidWest_info.Rds")
  } else if (update== FALSE){
    siteMidWest_info <- readRDS("data/siteMidWest_info.Rds")
  }
  return(siteMidWest_info)
}

# querydatas function
querydatas <- function(site = site,
                       pCode = pCode,
                       startDate = startDate,
                       endDate = endDate){
  
  querydata <- function(site){
    
    if (site$data_type_cd =="dv"){
      tmp_1 <- try(readNWISdv(siteNumbers = site$site_no, 
                              parameterCd = site$parm_cd,
                              startDate = site$startDate,
                              endDate = site$endDate), 
                   silent = FALSE)
      if (nrow(tmp_1) == 0) return(NULL)
      tmp_2 <- tmp_1 %>% 
        as_tibble %>% 
        select(Date, paste("X", site$parm_cd, site$stat_cd, sep = "_")) %>% 
        setNames(c("sample_dt", "result")) %>% 
        mutate(sample_dt = lubridate::as_date(sample_dt))
      
      
    } else if (site$data_type_cd =="qw"){
      tmp_1 <- try(readNWISqw(siteNumbers = site$site_no,
                              parameterCd = site$parm_cd,
                              startDate = site$startDate,
                              endDate = site$endDate,
                              expand = FALSE), 
                   silent = FALSE)
      if (nrow(tmp_1) == 0) return(NULL)
      tmp_2 <- tmp_1 %>% 
        as_tibble %>% 
        select(sample_dt, paste("p", site$parm_cd, sep = "")) %>% 
        setNames(c("sample_dt", "result")) %>% 
        mutate(sample_dt = lubridate::as_date(sample_dt))
    }
    
    return(tmp_2)
  }
  
  plan(multiprocess) # switch to parallel computing
  parm_data <- siteMidWest_select %>%
    filter(parm_cd == pCode) %>% 
    tibble::rowid_to_column("index") %>% 
    select(index, site_no, parm_cd, data_type_cd, stat_cd) %>% 
    mutate(startDate = startDate) %>% 
    mutate(endDate = endDate) %>% 
    nest(site = c(site_no, parm_cd, data_type_cd, stat_cd, startDate, endDate)) %>% 
    mutate(data = site %>% furrr::future_map(possibly(querydata, NA_real_))) %>% 
    unnest(site) %>% 
    select(-index); parm_data
  
  plan(sequential) # back to sequential computing
  
  return(parm_data)
}
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  query water   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv  Function   vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

thinshp <- function(shp){
  shp_st <- maptools::thinnedSpatialPoly(
    as(shp, "Spatial"), tolerance = 0.1, 
    minarea = 0.001, topologyPreserve = TRUE)
  shp <- st_as_sf(shp_st)
  return(shp)
}

site2sf <- function(df,id_cn ="id", Lon_cn = "longitude", Lat_cn = "latitude", crs= 4326){
  # cleanup data
  df_locs <- df %>% rename(longitude=Lon_cn, latitude=Lat_cn, id = id_cn)
  # convert to sf object
  df_locs <- st_as_sf(df_locs,
                      coords = c("longitude", "latitude"), # for point data
                      remove = F, # don't remove these lat/lon cols from df
                      crs = crs) # add projection (this is WGS84)
  return(df_locs)
}


# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  Function   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# vvvvvvvvvvvvvvvvvvvvvvvvvvvvvv point data preprocessing vvvvvvvvvvvvvvvvvvvvvvvvvvvv
crop_data_by_boundary <- function(data_sf, boundary = "ia_sf" ){
  
  site2sf_nest <- function(df, id_cn ="id", Lon_cn = "longitude", Lat_cn = "latitude", crs= 4326){
    #nest the unrenamed colummns
    df_locs <- df %>%
      rename(id = id_cn, longitude = Lon_cn, latitude = Lat_cn) %>%
      nest(data = c(-id, -longitude, -latitude))
    # convert to sf object
    df_locs <- st_as_sf(df_locs,
                        coords = c("longitude", "latitude"), # for point data
                        remove = F, # don't remove these lat/lon cols from df
                        crs = crs) # add projection (this is WGS84)
    return(df_locs)
  }
  
  sf2site_unnest <- function(df){
    df %>% 
      sf::st_drop_geometry() %>% 
      select(-region) %>% 
      unnest(cols = c(data))
  }
  
  data_sf <- data_sf %>% 
    site2sf_nest() %>% 
    st_intersection(get(boundary, envir = .GlobalEnv)) %>% 
    sf2site_unnest()
  return(data_sf)
}

crop_region_by_boundary <- function(region_sf, boundary = "ia_sf"){
  boundary <- get(boundary, envir = .GlobalEnv) %>% st_geometry()  
  region_sf <- get(region_sf, envir = .GlobalEnv) %>% sf::st_intersection(boundary)
  return(region_sf)
}

rescale_point_data <- function(x, to = c(1, 5)){
  x %>%  
    group_by(id) %>%
    mutate_at(names(x)[5] , scales::rescale, to = to) %>% 
    ungroup()
}
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ point data preprocessing ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv downscaling Function   vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

## spatiotemporal rescaling = function(data_sf, time_rescale, space_rescale, sf_geometry)
point_downscaling <- function(point_data, to_region = NULL, to_period = NULL, Spatial_downscaling = "mean", Temporal_downscaling = "mean"){
  # test: data <- prcp; sf_geometry <- county_sf; Spatial_downscaling = "mean"; Temporal_downscaling = "mean"
  # test: to_region = "county_sf"; to_period = "day"
  assign_region <- function(data, sf_geometry){
    site2sf_nest <- function(df, id_cn ="id", Lon_cn = "longitude", Lat_cn = "latitude", crs= 4326){
      
      # # nest the unrenamed colummns
      df_locs <- df %>%
        rename(id = id_cn, longitude = Lon_cn, latitude = Lat_cn) %>%
        nest(data = c(-id, -longitude, -latitude))
      # convert to sf object
      df_locs <- st_as_sf(df_locs,
                          coords = c("longitude", "latitude"), # for point data
                          remove = F, # don't remove these lat/lon cols from df
                          crs = crs) # add projection (this is WGS84)
      return(df_locs)
    }
    suppressWarnings({
      # convert df to sf object # intersect the site and polygon
      data_sf <- data %>%
        site2sf_nest() %>% 
        st_intersection(x = ., y = sf_geometry) %>%
        st_drop_geometry() %>%
        unnest(data) %>% 
        select(-id, -longitude, -latitude)
    })
    return(data_sf)
  }
  convert_to_period <- function(df, to_period){
    if (to_period == "day"){
      return(df)
    } else if (to_period == "month"){
      df <- df %>% 
        mutate(Date = Date %>% as.character() %>% 
                 stringr::str_sub(end = 7) %>% 
                 lubridate::ymd(truncated = 1))
    } else if(to_period == "year"){
      df <- df %>% 
        mutate(Date = Date %>% as.character() %>% 
                 stringr::str_sub(end = 4) %>% 
                 lubridate::ymd(truncated = 2)) 
    }
    return(df)
  }
  result_data <- point_data %>%
    get(envir = .GlobalEnv) %>% 
    assign_region(sf_geometry = to_region %>% get(envir = .GlobalEnv)) %>%
    group_by(Date, region) %>% 
    summarise_all(Spatial_downscaling, na.rm = TRUE) %>% # Spatial_downscaling
    ungroup() 
  if (to_period != "day"){
    result_data <- result_data %>% 
      convert_to_period(df = ., to_period = to_period) %>% 
      group_by(region, Date) %>% 
      summarise_all(Temporal_downscaling, na.rm = TRUE) %>% 
      ungroup()
  } 
  result_data <- result_data %>% 
    dplyr::rename(period = "Date") %>% 
    mutate(period = period %>% as.character %>% as.factor) %>% 
    mutate(region = region %>% as.factor) %>% 
    as.data.frame()
  return(result_data)
}

# downscaling initial dataset by pointDataSummary
initial_pointData_downscaling <- function(point_datas = NULL, periods = periods, regions = regions, Spatial_downscalings = NULL, Temporal_downscalings = NULL){
  # test: period <- periods[1]; region <- regions[1]
  for (period in periods){
    for (region in regions){
      cat("\n\n")
      scale_label <- paste0(stringr::str_sub(region, end = -4L), "X", period)
      print(paste0("pointdata_", scale_label))
      tibble(point_data = point_datas,
             Spatial_downscaling = Spatial_downscalings, 
             Temporal_downscaling = Temporal_downscalings) %>% 
        mutate(to_period = period, to_region = region) %>% 
        select(point_data, to_region, to_period, Spatial_downscaling, Temporal_downscaling) %>% 
        purrr::pmap(point_downscaling) %>% 
        purrr::reduce(full_join, by = c("region", "period")) %>% 
        mutate(period = as.factor(period)) %>% 
        mutate(region = as.factor(region)) %>% 
        select(region, period, everything()) %>% 
        arrange(region, period) %>% 
        assign(value = ., 
               x =  paste0("pointdata_", scale_label),
               envir = .GlobalEnv)
      cat("dim: ");
      get(paste0("pointdata_", scale_label), envir = .GlobalEnv) %>% dim %>% cat()
    }
  }
}

# downscaling multiple data by pointDataSummary
point_data_downscaling <- function(point_datas = NULL, periods = periods, regions = regions, Spatial_downscalings = NULL, Temporal_downscalings = NULL){
  # test: n= 1
  for (n in 1:length(point_datas)){
    point_data <- point_datas[n]
    Spatial_downscaling <- Spatial_downscalings[n]
    Temporal_downscaling <- Temporal_downscalings[n]
    
    # test: period <- periods[1]; region <- regions[1]
    for (period in periods){
      for (region in regions){
        cat("\n")
        scale_label <- paste0(stringr::str_sub(region, end = -4L), "X", period)
        print(paste0(point_data,"_", scale_label))
        tibble(point_data = point_datas,
               Spatial_downscaling = Spatial_downscalings, 
               Temporal_downscaling = Temporal_downscalings) %>% 
          mutate(to_period = period, to_region = region) %>% 
          select(point_data, to_region, to_period, Spatial_downscaling, Temporal_downscaling) %>% 
          purrr::pmap(point_downscaling) %>% 
          purrr::reduce(full_join, by = c("region", "period")) %>% 
          mutate(period = as.factor(period)) %>% 
          mutate(region = as.factor(region)) %>% 
          select(region, period, everything()) %>% 
          arrange(region, period) %>% 
          assign(value = ., 
                 x =  paste0(point_data,"_", scale_label),
                 envir = .GlobalEnv)
        cat(paste("# row: ", nrow(get(paste0(point_data,"_", scale_label), envir = .GlobalEnv) )))
      }
    }
  }
}




polygon_data_downscaling <- function(polygon_data_dict = polygon_data_dict , regional_division_df= NULL){
  # test: nr = 31
  convert_to_period <- function(df, period){
    if (period == "day"){
      return(df)
    } else if (period == "month"){
      df <- df %>% 
        mutate(period = period %>% as.character() %>% 
                 stringr::str_sub(end = 7) %>% 
                 lubridate::ymd(truncated = 1))
    } else if(period == "year"){
      df <- df %>% 
        mutate(period = period %>% as.character() %>% 
                 stringr::str_sub(end = 4) %>% 
                 lubridate::ymd(truncated = 2)) 
    }
    return(df)
  }
  for (nr in 1:nrow(polygon_data_dict)){
    # area_data name
    data <- polygon_data_dict[nr, ]$var
    # original scale
    orig_period <- polygon_data_dict[nr, ]$orig_period
    orig_region <- polygon_data_dict[nr, ]$orig_region
    # destinate scale
    period <- polygon_data_dict[nr, ]$dest_period
    region <- polygon_data_dict[nr, ]$dest_region
    scale_label <- paste0(region, "X", period)
    # downscaling method
    Spatialdownscaling <- polygon_data_dict[nr, ]$Spatialdownscaling
    Temporaldownscaling <- polygon_data_dict[nr, ]$Temporaldownscaling
    
    cat(paste0(data, " %>% ", Temporaldownscaling, "() %>% ", Spatialdownscaling, "() to ", region, "X", period))
    # obtain data
    tmp_var <- get(data, envir = .GlobalEnv) %>% 
      rename(period = orig_period) %>% 
      rename(region = orig_region) 
    
    # formulate the period col
    if (region != "day"){
    tmp_var <- tmp_var %>% 
      convert_to_period(df = ., period = period)
    } 
    # Temporal downscaling
      tmp_var <- tmp_var %>% 
        group_by(region, period) %>% 
        summarise_all("mean", na.rm = TRUE) %>% 
        ungroup() 
    # Spatial downscaling
    if (orig_region != region){
      tmp_var <- regional_division_df %>% 
        rename(dest_region = region) %>% 
        rename(region = orig_region) %>% 
        select(region, dest_region) %>% 
        right_join(tmp_var, by = "region") %>% 
        select(-region) %>% 
        rename(region = dest_region) %>% 
        group_by(region, period) %>% 
        summarise_all(Spatialdownscaling, na.rm = TRUE) %>% 
        ungroup() %>% 
        mutate_at(c("region", "period"), as.factor) 
    }
    
    # rm.na
    tmp_var <- tmp_var %>% 
      na.omit() %>% 
      mutate_at(c("region", "period"), as.factor) 
    
    # assign
    tmp_var %>% 
      assign(value = ., 
             x =  paste0(data, "_", scale_label),
             envir = .GlobalEnv)
    cat(paste("# row: ", nrow(tmp_var))); cat("\n")
  }
}

add_time_space_varible <- function(x, scale_label){
  x <- x %>% left_join(scale_label %>%  # add coordinate
              strsplit(split='X', fixed=TRUE) %>% 
              .[[1]] %>% 
              .[1] %>% 
              paste("_sf", sep = "") %>% 
              get(envir = .GlobalEnv) %>% 
              st_drop_geometry() %>% 
              cbind(scale_label %>% 
                      strsplit(split='X', fixed=TRUE) %>% 
                      .[[1]] %>% 
                      .[1] %>% 
                      paste("_sf", sep = "") %>% 
                      get(envir = .GlobalEnv) %>% 
                      st_geometry() %>% 
                      st_centroid() %>% 
                      st_coordinates() %>% 
                      as.data.frame() %>% 
                      `colnames<-`(c("longitude", "Latitude"))) %>% 
              mutate(longitude = longitude * 10 - min(longitude* 10)) %>% 
              mutate(Latitude = Latitude * 10 - min(Latitude* 10)) %>% 
              mutate_at(c("longitude", "Latitude"), round) %>% 
              as.tibble(),
            by = "region") %>% 
    mutate(time = period %>% yday()) %>% 
    mutate(time = time - min(time)) 
  return(x)
}

# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  downscaling Function   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  variableSelection Function   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# variable selection modelue
forwardSelection <- function(scale_label, rrp = 0.8){
  checkRemainRow <- function(new_var, tmp_joined){
    tmp_joined %>% 
      left_join(new_var, by = c("region", "period")) %>%
      na.omit() %>% 
      nrow()
  }
  # obtain initial joined dataset
  tmp_joined <- ls(pattern = paste0("pointdata_", scale_label), 
                   envir = .GlobalEnv) %>% 
    get(envir = .GlobalEnv) %>% 
    na.omit()
  # obtain nrow of initial joined dataset
  initialRow <- remainingRow <- tmp_joined %>% nrow(); initialRow
  # calculate the remainding nrow by adding one variable
  while (max(remainingRow) > initialRow*rrp){ # logical test with Remaining Row Percentage
    ## find add_var_index
    remainingRow <- ls(pattern = scale_label, 
                       envir = .GlobalEnv) %>% 
      .[!. %in% paste0("pointdata_", scale_label)] %>% 
      .[!. %in% paste0(names(tmp_joined),"_", scale_label)] %>% 
      map(get, envir = .GlobalEnv) %>% 
      map(~checkRemainRow(new_var = .x, tmp_joined= tmp_joined)) %>% 
      unlist(); remainingRow
    
    if (length(remainingRow) == 0) break
    
    add_var_index <- remainingRow %>% 
      which(x = (. == max(.))); add_var_index
    # join add_var_index
    add_var <- ls(pattern = scale_label, 
                  envir = .GlobalEnv) %>% 
      .[!. %in% paste0("pointdata_", scale_label)] %>% 
      .[!. %in% paste0(names(tmp_joined),"_", scale_label)] %>% 
      .[c(add_var_index)]; add_var
    
    tmp_joined <- add_var %>% 
      map(get, envir = .GlobalEnv) %>% 
      purrr::reduce(full_join, by = c("period", "region")) %>% 
      na.omit() %>% 
      right_join(tmp_joined, by = c("period", "region")) %>% 
      na.omit()
  }
  return(tmp_joined)
}



# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  variableSelection Function   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv Bayesian Network vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
# Bayesian Network
bnModeling <- function(tmp_joined){
  tmp_model <- tmp_joined %>%
    select(-region, -period) %>% 
    as.data.frame() %>% 
    boot.strength(data = ., algorithm = "hc") %>% 
    averaged.network 
}

plotMatrix_bn <- function(scale_label){
  cat("\n\n"); cat(paste0("joined_", scale_label))
  
  if (nrow(tmp_joined)!=0){
    get(paste0("model_", scale_label), 
        envir = .GlobalEnv) %>% 
      graphviz.plot(x = ., layout = "fdp", 
                    main = str_replace(scale_label, pattern = "X", replacement = " by "))
  } else {
    plot(0,type='n',axes=FALSE, main = "NA", xlab="", ylab="")
  }
}

plotMatrix_moralbn <- function(scale_label){
  cat("\n\n"); cat(paste0("joined_", scale_label))
  
  if (nrow(tmp_joined)!=0){
    get(paste0("model_", scale_label), 
        envir = .GlobalEnv) %>% 
      bnlearn::moral() %>% 
      graphviz.plot(x = ., layout = "fdp", 
                    main = str_replace(scale_label, pattern = "X", replacement = " by "))
  } else {
    plot(0,type='n',axes=FALSE, main = "NA", xlab="", ylab="")
  }
}


#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Bayesian Network ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

#vvvvvvvvvvvvvvvvvvvvvv exponential family Markov Networks vvvvvvvvvvvvvvvvvvvvvvvv
xmrfModeling <- function(tmp_joined){
  # my dataset
  simDat <- tmp_joined %>%   
    select_if(~n_distinct(.) > 1) %>% 
    select(-region, -period) %>% 
    as.matrix() %>% 
    t
  p = nrow(simDat)
  n = ncol(simDat)
  # Compute the optimal lambda
  lmax = lambdaMax(t(simDat))
  lambda = 0.01* sqrt(log(p)/n) * lmax
  # Run: stability = "bootstrap",  retains network edges that are estimated in more than 95 % (sth=0.95) of the 50 bootstrap repetitions (N=1000)
  model_lpgm <- XMRF(simDat, method="LPGM", N=1000, lambda.path=lambda, stability = "bootstrap", sth = 0.95)
  # Run: stability="STAR"
  # model_lpgm <- XMRF(simDat, method="LPGM", nlams=20, stability="STAR", th=0.001)
  
  return(model_lpgm)
}
plotMatrix_xmrf <- function(scale_label){
  
  cat("\n\n"); cat(paste0("joined_", scale_label))
  tmp_joined <- get(paste0("joined_", scale_label), envir = .GlobalEnv) %>%
    na.omit() %>% 
    select(-region, -period) 
  cm <- cor(tmp_joined)
  tmp_joined <- tmp_joined %>% 
    as.matrix() %>% 
    t
  tmp_model <- get(paste0("model_", scale_label), 
                   envir = .GlobalEnv) 
  if (nrow(tmp_joined)!=0){
    lpgm_igraph <- graph_from_adjacency_matrix(tmp_model$network[[1]], mode = "undirected", weighted = NULL,
                                               diag = TRUE, add.colnames = NULL, add.rownames = NA)
    
    allCor <- {}
    for (i in 1:nrow(tmp_joined)){
      
      tmp_cor <- cm[i,as.vector(lpgm_igraph[[i]][[1]])] %>% as.vector() 
      
      if (length(tmp_cor)==0) {
        allCor <- append(allCor, NA)
      } else {
        allCor <- append(allCor, tmp_cor)
      }
    }
    #plot(lpgm_igraph, vertex.label=rownames(tmp_joined), main = str_replace(scale_label, pattern = "X", replacement = " by "))
    lpgm_cluster <- cluster_fast_greedy(lpgm_igraph)
    cat("# of ", scale_label, " cluster :", length(lpgm_cluster))
    plot(lpgm_cluster, lpgm_igraph, 
         vertex.label=rownames(tmp_joined), 
         edge.label = ifelse(allCor > 0, "+", "-"), 
         edge.label.cex = 1.5,
         edge.label.font = 2,
         main = str_replace(scale_label, pattern = "X", replacement = ", "))
    
  } else {
    plot(0,type='n',axes=FALSE, main = "NA", xlab="", ylab="")
  }
}

#^^^^^^^^^^^^^^^^^^^^^^ exponential family Markov Networks ^^^^^^^^^^^^^^^^^^^^^^^^

#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv Summarise Network vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

# summarise a network
SummariseNetwork <- function(Scale){
  # test: Scale = "stateXyear"
  tmp_model <- ls(pattern = paste0("model_", Scale), envir = .GlobalEnv) %>% 
    get(envir = .GlobalEnv)
  tmp_igraph <- graph_from_adjacency_matrix(tmp_model$network[[1]], mode = "undirected", 
                                            weighted = NULL, diag = TRUE, 
                                            add.colnames = NULL, add.rownames = NA)
  tmp_joined <- ls(pattern = paste0("joined_", Scale), envir = .GlobalEnv) %>% 
    get(envir = .GlobalEnv) %>% na.omit() %>% select(-region, -period) 
  tibble("# of Data point" = nrow(tmp_joined),
         "# of Variable" = vcount(tmp_igraph), 
         "# of Arc" = ecount(tmp_igraph),
         "average degree" = mean(degree(tmp_igraph)),
         "average betweenness" = mean(betweenness(tmp_igraph)),
         "Assortativity Coefficient" = assortativity_degree(tmp_igraph, directed = FALSE))
}

degreeNetork <- function(Scale){
  dataGroup <- readr::read_csv("dataSummary.csv") %>% 
    select(Group, Variable) %>% fill(Group)
  
  # test: Scale = "stateXyear"
  tmp_model <- ls(pattern = paste0("model_", Scale), envir = .GlobalEnv) %>% 
    get(envir = .GlobalEnv)
  tmp_igraph <- graph_from_adjacency_matrix(tmp_model$network[[1]], mode = "undirected", 
                                            weighted = NULL, diag = TRUE, 
                                            add.colnames = NULL, add.rownames = NA)
  tmp_joined <- ls(pattern = paste0("joined_", Scale), envir = .GlobalEnv) %>% 
    get(envir = .GlobalEnv) %>% na.omit() %>% select(-region, -period) 
  tibble(Variable = names(tmp_joined),
         Degree = degree(tmp_igraph)) %>% 
    left_join(dataGroup,by = "Variable") %>% 
    select(Variable, Group, Degree) %>% 
    mutate(Scale = Scale)
}

betweennessNetork <- function(Scale){
  dataGroup <- readr::read_csv("dataSummary.csv") %>% 
    select(Group, Variable) %>% fill(Group)
  
  # test: Scale = "stateXyear"
  tmp_model <- ls(pattern = paste0("model_", Scale), envir = .GlobalEnv) %>% 
    get(envir = .GlobalEnv)
  tmp_igraph <- graph_from_adjacency_matrix(tmp_model$network[[1]], mode = "undirected", 
                                            weighted = NULL, diag = TRUE, 
                                            add.colnames = NULL, add.rownames = NA)
  tmp_joined <- ls(pattern = paste0("joined_", Scale), envir = .GlobalEnv) %>% 
    get(envir = .GlobalEnv) %>% na.omit() %>% select(-region, -period) 
  tibble(Variable = names(tmp_joined),
         Betweenness = betweenness(tmp_igraph)) %>% 
    left_join(dataGroup,by = "Variable") %>% 
    select(Variable, Group, Betweenness) %>% 
    mutate(Scale = Scale)
}
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Summarise Network ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv combine Edge vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv


combineEdge <- function(by_scale = "none"){
  
  # summarise a network
  extractEdges <- function(Scale){
    # test: Scale = "countyXmonth"
    spaceScale <- strsplit(Scale, split='X', fixed=TRUE) %>% .[[1]] %>% .[1]
    timeScale <- strsplit(Scale, split='X', fixed=TRUE) %>% .[[1]] %>% .[2]
    tmp_model <- ls(pattern = paste0("model_", Scale), envir = .GlobalEnv) %>% 
      get(envir = .GlobalEnv)
    tmp_joined <- ls(pattern = paste0("joined_", Scale), envir = .GlobalEnv) %>% 
      get(envir = .GlobalEnv) %>% na.omit() %>% select(-region, -period) 
    
    tmp_edges <- tmp_model$network[[1]]
    tmp_edges[lower.tri(tmp_edges)] <- NA
    tmp_edges %>% 
      `rownames<-`(names(tmp_joined)) %>% 
      `colnames<-`(names(tmp_joined)) %>% 
      as.data.frame() %>% 
      rownames_to_column(var = "V1") %>% 
      pivot_longer(cols = -V1, names_to = "V2", values_to = "edgeExist") %>% 
      filter(edgeExist == 1) %>%
      mutate(spaceScale = spaceScale, 
             timeScale = timeScale) %>% 
      select(spaceScale, timeScale, V1, V2, edgeExist)
  }
  
  scales <- ls(pattern = "model_", envir = .GlobalEnv) %>% str_remove("model_")
  combEdges <- scales %>% 
    map(extractEdges) %>% 
    reduce(rbind)
  
  if (by_scale == "spaceScale"){
    combEdges %>% 
      mutate(edge = paste0(V1,"-", V2)) %>% 
      mutate(spaceScale = spaceScale %>% fct_relevel("county", "AgDistrict", "state")) %>% 
      ggplot() +
      geom_bar(aes(fct_infreq(edge))) +
      coord_flip() +
      facet_grid(. ~ spaceScale) +
      ylab("Count") +
      xlab("Edge")
  }else if (by_scale == "timeScale"){
    combEdges %>% 
      mutate(edge = paste0(V1,"-", V2)) %>% 
      mutate(timeScale = timeScale %>% fct_relevel("day", "month", "year")) %>% 
      ggplot() +
      geom_bar(aes(fct_infreq(edge))) +
      coord_flip() +
      facet_grid(. ~ timeScale)+
      ylab("Count") +
      xlab("Edge")
  } else{
    combEdges %>%
      mutate(edge = paste0(V1,"-", V2)) %>% 
      ggplot() +
      geom_bar(aes(fct_infreq(edge))) +
      ylab("Count") +
      xlab("Edge") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
}
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ combine Edge ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

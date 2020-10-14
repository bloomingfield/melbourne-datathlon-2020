
build.ISD.weather.data = function(monthly.records = ( 30 * 24) * 0.25, years.of.interest = 2015:2019) { # once every four hours on average
  
  weather.stations = readr::read_csv('data/isd-lite/isd-history.csv')
  colnames(weather.stations) = make.names(colnames(weather.stations))
  weather.stations = weather.stations %>% filter(CTRY == 'AS')
  
  weather.station.volume = readr::read_csv('data/isd-lite/isd-inventory.csv')
  colnames(weather.station.volume) = make.names(colnames(weather.station.volume))
  weather.station.volume = weather.station.volume %>% filter(YEAR %in% years.of.interest)
  
  
  
  month.vec = c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')
  wsv = weather.station.volume %>% filter(across(all_of(month.vec), ~ .x > monthly.records))
  wsv = wsv %>% filter(YEAR %in% years.of.interest)
  wsv = wsv %>% group_by(USAF, WBAN) %>% summarise(count=n())
  wsv = wsv %>% filter(count == length(years.of.interest))
  
  weather.stations = weather.stations %>% filter(paste0(USAF, WBAN) %in% paste0(wsv$USAF, wsv$WBAN))
  
  states.shapefile = shapefile('data/abs/shapefile/STE_2016_AUST.shp')
  
  points = SpatialPoints(weather.stations[, c('LON', 'LAT')], proj4string = CRS("+proj=longlat"))
  points = spTransform(points, states.shapefile@proj4string)
  
  
  weather.spatial = weather.stations %>% bind_cols(raster::extract(states.shapefile, points))
  weather.spatial$STATE = NULL
  weather.spatial$ICAO = NULL
  print(weather.spatial[!complete.cases(weather.spatial), ])
  weather.spatial = weather.spatial %>% drop_na()
  
  p = ggplot()
  p = p + borders('world', xlim = c(110, 160), ylim = c(-50, -10))
  p = p + geom_point(data=weather.stations, aes(x=LON, y=LAT))
  p = p + scale_size_area()
  p = p +  coord_fixed()
  print(p)
  saveRDS(weather.spatial, file='output/ISDweatherstations.RDS')
} 

# =============================================================================
# =============================================================================

download.ISD.weather.data = function(weather.spatial, years=2015:2020, cores=4) {
  
  for (i in 1:dim(weather.spatial)[1]) {
    print(i)
    row = weather.spatial[i, ]
    code = paste0(row$USAF, '-', row$WBAN)
    data = importNOAA(code = code, year=years, hourly=T, n.cores=cores, path=paste0('data/isd-lite/output'))
  }
}

# =============================================================================
# =============================================================================

process.ISD.weather.data = function(weather.spatial, max.linear.interp.gap = 6, max.seas.ma.gap = 720) {
  
  for (i in 1:dim(weather.spatial)[1]) {
    print(i)
    row = weather.spatial[i, ]
    code = paste0(row$USAF, '-', row$WBAN)
    read.files = list.files('data/isd-lite/output', pattern=paste0(code,'*'), full.names = T)
    weather.isd.data = bind_rows(lapply(read.files, readRDS))
    
    weather.isd.data = weather.isd.data %>% arrange(date) %>% mutate(id = code)
    
    # Now to interpolate between missing hours
    weather.isd.data = weather.isd.data %>% group_by(id) %>%
      mutate(air_temp_interp = na_interpolation(air_temp, maxgap = max.linear.interp.gap, option = 'linear')) %>% # maximum 6 hour gap
      mutate(RH_interp = na_interpolation(RH, maxgap = max.linear.interp.gap, option = 'linear')) # maximum 6 hour gap
    
    # If still missing use a moving average over a longer period of time...
    weather.isd.data = weather.isd.data %>% group_by(id) %>%
      mutate(air_temp_interp2 = na_seasplit(air_temp_interp, maxgap = max.seas.ma.gap, find_frequency = TRUE, algorithm = "ma", k = 30, weighting='simple')) %>% # maximum 6 hour gap
      mutate(RH_interp2 = na_seasplit(RH_interp, maxgap = max.seas.ma.gap, find_frequency = TRUE, algorithm = "ma", k = 30, weighting='simple')) # maximum 6 hour gap\
    saveRDS(weather.isd.data, file=paste0('data/isd-lite/processed/',code,'.RDS'))
  }
}


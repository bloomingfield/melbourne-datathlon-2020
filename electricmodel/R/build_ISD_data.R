
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

untar.ISD.weather.data = function(weather.spatial) {
  
  input.dir = 'data/isd-lite/raw/'
  output.dir = 'data/isd-lite/untar/'
  
  file.names = list.files(path=input.dir)
  
  foreach(i=1:length(file.names)) %dopar% {
    R.utils::gunzip(paste0(input.dir, file.names[i]), destname=paste0(output.dir, file.names[i], '.txt'), remove=F, overwrite=T)
  }
}

# =============================================================================
# =============================================================================

process.ISD.weather.data = function(weather.spatial) {
  
  output.dir = 'data/isd-lite/untar/'
  
  weather.isd.data = foreach(i= 1:dim(weather.spatial)[1]) %dopar% {
    print(i)
    row = weather.spatial[i, ]
    read.files = list.files(path = output.dir, pattern = paste0(row$USAF, '-', row$WBAN), full.names = T)
    data = read.files %>%  map_df(~read_table(., col_names=F))
    colnames(data) = c('year', 'month', 'day', 'hour', 'air.temp', 'dew.point.temp', 'sea.level.pressure', 'wind.direction', 'wind.speed', 'sky.coverage.code', 'hourly.precip', 'six.hourly.precip')
    data$station = row$STATION.NAME
    data$lat = row$LAT
    data$lon = row$LON
    data$state = row$STE_NAME16
    data$USAF = row$USAF
    data$WBAN = row$WBAN
    data
  }
  weather.isd.data = bind_rows(weather.isd.data)
  saveRDS(weather.isd.data, file='data/isd-lite/raw.rds')
}

# =============================================================================
# =============================================================================

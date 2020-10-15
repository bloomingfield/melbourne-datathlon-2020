

build.state.data = function(select.state = 'NSW') {
  
  electricity.data = readRDS('output/energy.data.RDS')
  weather.spatial = readRDS('output/ISDweatherstations.RDS')
  
  electricity.data$SETTLEMENTDATE = ymd_hms(electricity.data$SETTLEMENTDATE)
  electricity.data = electricity.data %>% filter(REGION == paste0(select.state, '1'))
  
  if (select.state == 'NSW') {
    state.name = 'New South Wales'
  } else if (select.state == 'VIC') {
    state.name = 'Victoria'
  }
  
  weather.spatial = weather.spatial %>% filter(STE_NAME16 == state.name)
  
  weather.list = list()
  for (i in 1:dim(weather.spatial)[1]) {
    row = weather.spatial[i, ]
    data = readRDS(paste0('data/isd-lite/processed/',row$USAF, '-', row$WBAN, '.RDS'))
    if (sum(is.na(data$RH_interp2)) == 0 & sum(is.na(data$air_temp_interp2)) == 0) {
      weather.list[[i]] = data
    }
  }
  weather.ISD = bind_rows(weather.list)
  stations = weather.ISD %>% select(longitude, latitude, station) %>% distinct()
  weather.spatial = weather.spatial %>% filter(paste0(USAF, '-', WBAN) %in% stations$id)
  
  if (F) {
    p = ggplot()
    p = p + borders('world', xlim = c(110, 160), ylim = c(-50, -10))
    p = p + geom_point(data=stations, aes(x=longitude, y=latitude))
    p = p + scale_size_area()
    p = p +  coord_fixed()
    print(p)
  }
  
  weather.ISD.use = weather.ISD %>% select(id, date, air_temp_interp2, RH_interp2)
  weather.ISD.use = weather.ISD.use %>% pivot_wider(names_from = id, values_from = c(air_temp_interp2, RH_interp2))
  weather.ISD.use$date = as.POSIXct(weather.ISD.use$date, tz = "UTC")
  
  electricity.data.use = electricity.data %>% select(TOTALDEMAND, SETTLEMENTDATE)
  electricity.data.use$SETTLEMENTDATE = force_tz(electricity.data.use$SETTLEMENTDATE, tz = "Australia/Brisbane")
  electricity.data.use = electricity.data.use %>% rename(date=SETTLEMENTDATE)
  
  weather.ISD.use$date = with_tz(weather.ISD.use$date, tzone = "Australia/Brisbane")

  state.data = electricity.data.use %>% left_join(weather.ISD.use, by='date')
  state.data = state.data %>% mutate(
    across(starts_with("RH"), ~na_interpolation(.x, maxgap = 2, option = 'linear')),
    across(starts_with("air_temp"), ~na_interpolation(.x, maxgap = 2, option = 'linear'))
    )
  
  state.data = state.data %>% drop_na()
  
  dd = list(weather.spatial=weather.spatial, state.data=state.data)
  saveRDS(dd, file=paste0('output/',select.state,'.RDS'))
}

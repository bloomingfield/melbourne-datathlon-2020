

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
  
  dd = list(weather.spatial=weather.spatial, state.data=state.data, weather.ISD=weather.ISD)
  saveRDS(dd, file=paste0('output/',select.state,'.RDS'))
}


# =======================================================

add.solar.data = function(select.state = 'NSW',years.weather =2015:2020) {
  
  dd.list = readRDS(file=paste0('output/',select.state,'.RDS'))
  
  res = raster(paste0('data/silo/2015.radiation.nc'))
  australia.points = data.frame(rasterToPoints(res, fun=function(x){!is.na(x)}))
  australia.points$cell = cellFromXY(res[[1]], australia.points[, c(1,2)])
  
  locations.use = dd.list$weather.spatial
  locations.use$lat.closest = 0
  locations.use$long.closest = 0
  locations.use$closest.cell = 0
  locations.use$id = paste0(locations.use$USAF,'-',locations.use$WBAN)
  for (i in 1:length(locations.use$LON)) {
    distance = ((australia.points[,1]-locations.use$LON[i])^2+(australia.points[,2]-locations.use$LAT[i])^2)^0.5
    closest.point = australia.points[which.min(distance),]
    locations.use$lat.closest[i] = closest.point[2]
    locations.use$long.closest[i] = closest.point[1]
    locations.use$closest.cell[i] = closest.point$cell
  }
  
  plot(res,axes = FALSE,legend=FALSE)
  points(locations.use[, c('LON', 'LAT')])
  points(locations.use[, c('long.closest', 'lat.closest')])
  
  solar.list = list()
  for (i in 1:length(years.weather)) {
    print(years.weather[i])
    solar.list[[i]] = extract.solar.from.raster(locations.use, year=years.weather[i], variable='radiation')
  }
  solar.list = bind_rows(solar.list)
  
  solar = pivot_longer(solar.list, make.names(locations.use$id), names_to='id', values_to='radiation')
  
  solar = solar %>% mutate(id = gsub('X', '', id), id = gsub('.', '-', id, fixed=T), id =paste0('radiation_',id),
                   date = gsub('X', '', date), date = gsub('.', '-', date, fixed=T), date=ymd(date))
  
  
  solar.use = pivot_wider(solar, names_from='id', values_from='radiation')
  
  dd = dd.list$state.data
  dd <- dd %>% mutate(hour = (hour(date)*60+minute(date))/60, datetime=date, date=as_date(dd$date), month.year = paste0(year(date),'-',month(date)))
  dd = dd %>% left_join(solar.use, by='date')
  dd.list$state.data = dd
  saveRDS(dd.list, file=paste0('output/',select.state,'.RDS'))
}

# =======================================================

add.holidays.and.average = function(select.state = 'NSW', years.weather =2015:2020) {
  dd.list = readRDS(file=paste0('output/',select.state,'.RDS'))
  
  
  dd = dd.list$state.data
  
  
  # ===========================================================================
  # solar intensity time of day addition
  
  lat = mean(dd.list$weather.spatial$LAT)
  lon = mean(dd.list$weather.spatial$LON)
  
  dd.dates = dd %>% select(date) %>% distinct()
  
  guess.solar.intensity = function(hour, solarNoon, sunrise, sunset) {
    
    sun.rise.val = pmax(sin((hour-sunrise)*pi/2/(solarNoon-sunrise)), 0)
    sun.set.val = pmax(sin((sunset - hour)*pi/2/(sunset-solarNoon)), 0)
    
    res = sun.rise.val
    res[hour > solarNoon] = sun.set.val[hour > solarNoon]
    return(res)
  }
  # xseq = seq(0,24)
  # test = guess.solar.intensity(xseq, 12, 6, 20)
  # plot(xseq, test)
  
  sun.times = getSunlightTimes(date = dd.dates$date, lat = lat, lon = lon, tz='Australia/Brisbane')
  sun.times = sun.times %>%  select(date, solarNoon, sunrise, sunset)
  sun.times = sun.times %>% mutate(solarNoon = hour(solarNoon) + minute(solarNoon)/60, 
                       sunrise = hour(sunrise) + minute(sunrise)/60,
                       sunset = hour(sunset) + minute(sunset)/60)
  
  dd = dd %>% left_join(sun.times, by='date', suffix=c('', ''))
  dd = dd  %>% mutate(solar.intensity = guess.solar.intensity(hour, solarNoon, sunrise, sunset),
                      across(starts_with("radiation"), ~.x*solar.intensity, .names='{.col}'))
  
  # dd <- dd %>% mutate(across(starts_with("radiation"), ~.x*solar.intensity))
  # ===========================================================================
  # lagged predictors
  dd <- dd %>% mutate(across(starts_with("RH"), ~lag(.x, 48), .names='1daylag_{.col}'),
                      across(starts_with("air_temp"), ~lag(.x, 48), .names='1daylag_{.col}'),
                      across(starts_with("radiation"), ~lag(.x, 48), .names='1daylag_{.col}'),
                      across(starts_with("RH"), ~lag(.x, 48*7), .names='7daylag_{.col}'),
                      across(starts_with("air_temp"), ~lag(.x, 48*7), .names='7daylag_{.col}'),
                      across(starts_with("radiation"), ~lag(.x, 48*7), .names='7daylag_{.col}')
  )
  
  dd <- dd %>% mutate(across(starts_with("RH"), ~frollmean(.x, 48), .names='24hrmean_{.col}'),
                      across(starts_with("air_temp"), ~frollmean(.x, 48), .names='24hrmean_{.col}'),
                      across(starts_with("radiation"), ~frollmean(.x, 48), .names='24hrmean_{.col}'),
                      across(starts_with("RH"), ~frollmean(.x, 48*7), .names='weekmean_{.col}'),
                      across(starts_with("air_temp"), ~frollmean(.x, 48*7), .names='weekmean_{.col}'),
                      across(starts_with("radiation"), ~frollmean(.x, 48*7), .names='weekmean_{.col}')
  )
  # ===========================================================================
  # holidays
  
  holidays = tsibble::holiday_aus(years.weather, state=select.state)
  dd <- dd %>% left_join(holidays, by='date', suffix=c('', ''))
  dd <- dd %>% mutate(holiday = if_else(is.na(holiday), 0, 1))
  dd = dd %>% drop_na()
  
  dd.dates = dd %>% select(date) %>% distinct()
  dd.dates$nearest.holiday.distance = 0
  for (i in 1:dim(dd.dates)[1]) {
    # print(i)
    dd.dates$nearest.holiday.distance[i] = min(abs(dd.dates$date[i]-holidays$date))
  }
  dd = dd %>% left_join(dd.dates, by='date', suffix=c('', ''))
  
  # ===========================================================================
  # Other time dummy variables
  dd = dd %>% mutate(day.of.week = lubridate::wday(date, label=T), month = lubridate::month(date, label=T), year=lubridate::year(date), day.of.year = yday(date))
  
  week.days = c('Thu', 'Fri', 'Mon', 'Tue', 'Wed')
  dd = dd %>% mutate(is.weekday = if_else(day.of.week %in% week.days, 1, 0))
  # ===========================================================================
  # 2D Time
  dd = dd %>% mutate(hour.x = sin((360/24)*hour), hour.y = cos((360/24)*hour),
                     month.x = sin((360/12)*as.numeric(month)), month.y = cos((360/12)*as.numeric(month))
                          )
  # ===========================================================================
  dd.list$state.data = dd
  saveRDS(dd.list, file=paste0('output/',select.state,'_mod.RDS'))
}


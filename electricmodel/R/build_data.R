
build.data = function() {
    
  # ================================================================================
  # Getting electricity data in nice format
  # ================================================================================
  
  electricity.files = list.files('data/aemo/price_demand', full.names = T)
  electricity.data = lapply(electricity.files, read_csv)
  electricity.data = bind_rows(electricity.data)
  saveRDS(electricity.data, file='output/energy.data.RDS')
  
  # ================================================================================
  # getting population data in a nice format
  # ================================================================================
  
  population = read_csv('data/abs/population_cities.csv')
  # population = population %>% filter(...2 %in% cities.to.explore)
  population = pivot_longer(population, where(is.numeric), names_to='year', values_to='population')
  population$year = ymd(paste0(population$year, '-06-30'))
  population = population %>% filter(year == ymd('2019-06-30'))
  population$country = 'Australia'
  
  # ================================================================================
  
  cities = paste0(population$cities.geocode, ', ', population$country)
  cities.list = list()
  for (i in 1:length(cities)) {
    print(i)
    cities.list[[i]] = geo(address = cities[i], method = 'osm', full_results =T)
  }
  cities.r = bind_rows(cities.list)
  cities.r = cities.r %>% relocate(display_name)
  
  regional.cities.location = population %>% bind_cols(cities.r)
  saveRDS(regional.cities.location, file='output/population_cities.RDS')
  
  # ================================================================================
  # getting weather data in nice format
  # ================================================================================
  
  years.weather = 2010:2020
  australia.extent = extent(110, 160, -50, -10)
  
  locations.use = regional.cities.location
  locations.use = locations.use %>% drop_na()
  
  res = stack(paste0('data/noaa/tmin.2015.nc'))
  australia.crop = crop(res[[1]], australia.extent)
  australia.points = data.frame(rasterToPoints(australia.crop))
  australia.points$cell = cellFromXY(res[[1]], australia.points[, c(1,2)])
  
  locations.use$lat.closest = 0
  locations.use$long.closest = 0
  locations.use$closest.cell = 0
  for (i in 1:length(locations.use$year)) {
    distance = ((australia.points[,1]-locations.use$long[i])^2+(australia.points[,2]-locations.use$lat[i])^2)^0.5
    closest.point = australia.points[which.min(distance),]
    locations.use$lat.closest[i] = closest.point[2]
    locations.use$long.closest[i] = closest.point[1]
    locations.use$closest.cell[i] = closest.point$cell
  }
  
  plot(australia.crop,axes = FALSE,legend=FALSE)
  points(locations.use[, c('long', 'lat')])
  points(locations.use[, c('long.closest', 'lat.closest')])
  
  temp.min.list = list()
  temp.max.list = list()
  precipitation.list = list()
  for (i in 1:length(years.weather)) {
    print(years.weather[i])
    temp.min.list[[i]] = extract.weather.from.raster(locations.use, year=years.weather[i], variable='tmin')
    temp.max.list[[i]] = extract.weather.from.raster(locations.use, year=years.weather[i], variable='tmax')
    precipitation.list[[i]] = extract.weather.from.raster(locations.use, year=years.weather[i], variable='precip')
  }
  temp.min = bind_rows(temp.min.list)
  temp.max = bind_rows(temp.max.list)
  precipitation = bind_rows(precipitation.list)
  
  temp.max = pivot_longer(temp.max, make.names(locations.use$cities.name), names_to='city', values_to='tmax')
  temp.min = pivot_longer(temp.min, make.names(locations.use$cities.name), names_to='city', values_to='tmin')
  precipitation = pivot_longer(precipitation, make.names(locations.use$cities.name), names_to='city', values_to='precip')
  
  weather = left_join(precipitation, temp.min)
  weather = left_join(weather, temp.max)
  saveRDS(weather, file='output/weather.RDS')
  
  # ================================================================================
  # getting state total population level data
  # ================================================================================
  state.vector = c('NSW', 'VIC', 'SA', 'WA', 'TAS', 'NT', 'ACT')
  
  population.state = read_excel('data/abs/population_states.xls', 2)
  colnames(population.state) = make.names(colnames(population.state))
  population.state = population.state %>% select(
    ...1,
    Estimated.Resident.Population....Persons....New.South.Wales.., 
    Estimated.Resident.Population....Persons....Victoria..,
    Estimated.Resident.Population....Persons....South.Australia..,
    Estimated.Resident.Population....Persons....Western.Australia..,
    Estimated.Resident.Population....Persons....Tasmania..,
    Estimated.Resident.Population....Persons....Northern.Territory..,
    Estimated.Resident.Population....Persons....Australian.Capital.Territory..
    )
  colnames(population.state) = c('date', state.vector)
  population.state = population.state[10:length(population.state$date), ]
  population.state = population.state %>% mutate_all(as.numeric)
  population.state$date = as.Date(population.state$date, origin="1900-01-01")-days(2)+months(1)
  
  population.state = pivot_longer(population.state, state.vector, names_to='state', values_to='population')
  
  saveRDS(population.state, file='output/population_state.RDS')

}
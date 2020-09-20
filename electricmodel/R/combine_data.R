
combine.data = function(smallest.city = 20000) {
  
  energy.data = readRDS('output/energy.data.RDS')
  population.cities.data = readRDS('output/population_cities.RDS')
  population.states.data = readRDS('output/population_state.RDS')
  weather.data = readRDS('output/weather.RDS')
  
  energy.data = energy.data %>% mutate(date_time=ymd_hms(SETTLEMENTDATE), date=date(date_time))
  energy.data = energy.data %>% group_by(date, REGION) %>% summarise(demand = sum(TOTALDEMAND))
  energy.data = energy.data %>% rename(state=REGION) %>% mutate(state=substr(state,1,nchar(state)-1))
  
  population.cities.data = population.cities.data %>% drop_na() %>% filter(population > smallest.city) %>% mutate(cities.name = make.names(cities.name))
  population.cities.data = population.cities.data %>% select(cities.name, State) %>% rename(city = cities.name)
  
  weather.data = weather.data %>% mutate(date = as.Date(date, "X%Y.%m.%d"))
  weather.data = left_join(population.cities.data, weather.data)
  weather.data= weather.data %>% rename(state=State)
  
  all.data = left_join(weather.data, population.states.data)
  all.data = left_join(all.data, energy.data)
  
  all.data = all.data %>% drop_na(demand)
  saveRDS(all.data, file='output/alldata.RDS')
  # =========================================
}

build.state.data = function(select.state = 'NSW') {
  all.data = readRDS('output/alldata.RDS')
  state.data = all.data %>% filter(state == select.state)
  state.data = state.data %>% pivot_wider(names_from = city, values_from = c(precip, tmin, tmax))
  
  state.data$population = na.approx(state.data$population, x=state.data$date, na.rm = FALSE)
  last.population = state.data$population[state.data$date == ymd('2020-01-01')]
  state.data$population[is.na(state.data$population)] = last.population
  
  # plot(state.data$date, state.data$population)
  
  saveRDS(state.data, file=paste0('output/',select.state,'.RDS'))
}

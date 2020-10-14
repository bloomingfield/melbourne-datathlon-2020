

build.state.data = function(select.state = 'NSW') {
  
  electricity.data = readRDS('output/energy.data.RDS')
  
  all.data = readRDS('output/alldata.RDS')
  state.data = all.data %>% filter(state == select.state)
  state.data = state.data %>% pivot_wider(names_from = city, values_from = c(precip, tmin, tmax))
  
  state.data$population = na.approx(state.data$population, x=state.data$date, na.rm = FALSE)
  last.population = state.data$population[state.data$date == ymd('2020-01-01')]
  state.data$population[is.na(state.data$population)] = last.population
  
  # plot(state.data$date, state.data$population)
  
  saveRDS(state.data, file=paste0('output/',select.state,'.RDS'))
}

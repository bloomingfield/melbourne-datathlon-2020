library(raster)
library(tidyverse)
library(readxl)
library(lubridate)
library(data.table)
library(XML)
library(tidygeocoder)




# ================================================================================
# ================================================================================

cities.to.explore = c('Melbourne', 'Adelaide', 'Brisbane', 'Sydney', 'Canberra')
cities.longitude = c(-37.81, -34.92, -27.47, -33.87, -35.27)
cities.latitude = c(144.96, 138.62, 153.03, 151.19, 149.13)
cities.energy.providers = list(c('CITIPOWER', 'VICAGL', 'UNITED'), c('UMPLP'), c('ENERGEX'), c('ENERGYAUST', 'INTEGRAL'), c('ACTEWAGL'))

meta.data = tibble(cities = cities.to.explore, longitude = cities.longitude, latitude = cities.latitude, energy.distributors = cities.energy.providers)
meta.data = unnest(meta.data, cols=c(energy.distributors))

#ENERGEX - QLD Brisbane and surrounds - https://www.energex.com.au/about-us/company-information/our-network/the-south-east-queensland-electricity-network

#ACTEWAGL - ACT https://www.aer.gov.au/networks-pipelines/service-providers-assets/evoenergy-electricity-distribution-network

#CITIPOWER - VIC melbroune city - https://www.aer.gov.au/networks-pipelines/service-providers-assets/citipower
#VICAGL - Melbourne north and south west VIC - https://www.aer.gov.au/networks-pipelines/service-providers-assets/jemena
#UNITED - Melbroune south VIC - https://www.aer.gov.au/networks-pipelines/service-providers-assets/united-energy

#ENERGYAUST - Sydney+newcastle NSW - https://electricitywizard.com.au/electricity/distributors/nsw/ausgrid/
#INTEGRAL - Sydney other NSW - https://electricitywizard.com.au/electricity/distributors/nsw/ausgrid/

#UMPLP - All SA - https://en.wikipedia.org/wiki/SA_Power_Networks

#AURORA - tasmania - https://www.aer.gov.au/networks-pipelines/service-providers-assets/aurora-energy-electricity-distribution-network
#COUNTRYENERGY - NSW - https://www.aer.gov.au/networks-pipelines/service-providers-assets/country-energy
#ERGON1 - QLD other - https://www.ergon.com.au/network/help-and-support/about-us/who-we-are/service-regions-and-depot-map
#POWERCOR - other VIC- https://www.greenwire.com.au/visitor/list-of-electricity-retailers-distributors-in-victoria/
#TXU - AusNet- west VIC - https://www.greenwire.com.au/visitor/list-of-electricity-retailers-distributors-in-victoria/

# ================================================================================
# getting electricity data in a nice format
# ================================================================================
# easy stuff...
years = 2015:2019
year.list = list()
for (i in 1:length(years)) {
  year.list[[i]] = read_csv(paste0('data/aemo_load/',years[i],'_nslp.csv'))
}
energy.data = bind_rows(year.list)


# not so easy stuff...
weekly.files = list.files(path='data/aemo_load/', pattern='*.xml', full.names = T)
weekly.list = list()
for ( i in 1:length(weekly.files)) {
  data = xmlParse(file= weekly.files[i])
  rootnode <- xmlRoot(data)
  csv.data = rootnode[[2]][[1]][[1]][[2]][[1]][[1]]
  csv.data = xmlValue(csv.data)
  weekly.list[[i]] = read.csv(text=csv.data)
}
weekly.res = bind_rows(weekly.list)
energy.all = rbind(energy.data, weekly.res)

energy.all = energy.all %>% filter(ProfileName == 'NSLP')
energy.all = energy.all %>% mutate(total.nslp.load = rowSums(dplyr::select(., starts_with("Period"))))
energy.all = energy.all %>% select(total.nslp.load, SettlementDate, ProfileArea)

saveRDS(energy.all, file='output/energy.data.Rdata')


# energy.data.p = energy.data[1:300, ]
# plot(energy.data.p$SettlementDate, energy.data.p$total.nslp.load)

# ================================================================================
# getting population data in a nice format
# ================================================================================

population = read_excel('data/abs/population_cities.xls', 2, skip=6)
# population = population %>% filter(...2 %in% cities.to.explore)
columns = population$...2[3:112]
rows = 2009:2019
population = population[3:112, 3:13]
population = t(population)
colnames(population) = columns
population = as_tibble(population)
population$date = ymd(paste0(rows, '-6-30'))
population = pivot_longer(population, columns, names_to='city', values_to='population')
population$population = as.numeric(population$population)
population = population %>% filter(date == ymd('2019-06-30'))
population$country = 'Australia'

# ================================================================================

cities = paste0(population$city, ', ', population$country)
cities.list = list()
for (i in 1:length(cities)) {
  print(i)
  cities.list[[i]] = geo(address = cities[i], method = 'osm', full_results =T)
}
cities.r = bind_rows(cities.list)
cities.r= cities.r%>% relocate(display_name)

regional.cities.location = population %>% bind_cols(cities.r)
saveRDS(regional.cities.location, file='output/population_cities.RDS')

# ================================================================================
# getting weather data in nice format
# ================================================================================
years.weather = 2015:2020

temp.min.list = list()
temp.max.list = list()
precipitation.list = list()
for (i in 1:length(years.weather[1:2])) {
  print(years.weather[i])
  res = stack(paste0('data/noaa/tmin.',years.weather[i],'.nc'))
  temp.min.list[[i]] = tibble::rownames_to_column(data.frame((t(raster::extract(res, meta.data[, c('latitude', 'longitude')])))), "date")
  colnames(temp.min.list[[i]]) = c('date', meta.data$energy.distributors)
  
  res = stack(paste0('data/noaa/tmax.',years.weather[i],'.nc'))
  temp.max.list[[i]] = tibble::rownames_to_column(data.frame(t(raster::extract(res, meta.data[, c('latitude', 'longitude')]))), "date")
  colnames(temp.max.list[[i]]) = c('date', meta.data$energy.distributors)
  
  res = stack(paste0('data/noaa/precip.',years.weather[i],'.nc'))
  precipitation.list[[i]] = tibble::rownames_to_column(data.frame(t(raster::extract(res, meta.data[, c('latitude', 'longitude')]))), "date")
  colnames(precipitation.list[[i]]) = c('date', meta.data$energy.distributors)
}
temp.min = rbindlist(temp.min.list)
temp.max = bind_rows(temp.max.list)
precipitation = bind_rows(precipitation.list)

temp.max = pivot_longer(temp.max, meta.data$energy.distributors, names_to='distributor', values_to='tmax')
temp.min = pivot_longer(temp.min, meta.data$energy.distributors, names_to='distributor', values_to='tmin')
precipitation = pivot_longer(precipitation, meta.data$energy.distributors, names_to='distributor', values_to='precip')

weather = left_join(precipitation, temp.min)
weather = left_join(weather, temp.max)
save(weather, file='output/weather.Rdata')

# ================================================================================
# getting indicator data in nice format
# ================================================================================




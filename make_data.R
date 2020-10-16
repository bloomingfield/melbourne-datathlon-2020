devtools::document('electricmodel')

min.year = 2015
max.year = 2020
download.files = F

build.ISD.weather.data(monthly.records = ( 30 * 24) * 0.25, years.of.interest = 2015:2019)
weather.spatial = readRDS('output/ISDweatherstations.RDS')

if (download.files == T) {
  download.demand.data(year.list = min.year:max.year, max.date = '2020-10-15')
  download.silo.data(years = min.year:max.year)
  download.ISD.weather.data(weather.spatial, years=min.year:max.year, cores=3)
  process.ISD.weather.data(weather.spatial, max.linear.interp.gap = 6, max.seas.ma.gap = 720)
}

build.price.demand.data()

build.state.data(select.state = 'NSW')
add.solar.data(select.state = 'NSW',years.weather =min.year:max.year)

build.state.data(select.state = 'VIC')
add.solar.data(select.state = 'VIC',years.weather =min.year:max.year)

add.holidays.and.average(select.state = 'NSW',years.weather =min.year:max.year)
add.holidays.and.average(select.state = 'VIC',years.weather =min.year:max.year)

if (F) {
  test = readRDS(paste0('output/','NSW','.RDS'))
  test = readRDS(paste0('output/','NSW_mod','.RDS'))
  acf(test$state.data$TOTALDEMAND)
  tail(test$state.data)
  library(forecast)
  forecast::ggAcf(test$state.data$TOTALDEMAND, lag.max=100*5)
}


# 
# download.demand.data(year.list = 2010:2020) 
# build.data()
# combine.data(smallest.city = 20000)
# 
# build.state.data(select.state = 'NSW')
# build.state.data(select.state = 'VIC')

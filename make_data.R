devtools::document('electricmodel')

min.year = 2015
max.year = 2020
download.files = F

build.ISD.weather.data(monthly.records = ( 30 * 24) * 0.25, years.of.interest = 2015:2019)
weather.spatial = readRDS('output/ISDweatherstations.RDS')

if (download.files == T) {
  download.demand.data(year.list = min.year:max.year, max.date = '2020-10-01')
  download.ISD.weather.data(weather.spatial, years=min.year:max.year, cores=3)
  process.ISD.weather.data(weather.spatial, max.linear.interp.gap = 6, max.seas.ma.gap = 720)
}

build.price.demand.data()

build.state.data(select.state = 'NSW')
build.state.data(select.state = 'VIC')

# 
# download.demand.data(year.list = 2010:2020) 
# build.data()
# combine.data(smallest.city = 20000)
# 
# build.state.data(select.state = 'NSW')
# build.state.data(select.state = 'VIC')

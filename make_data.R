devtools::document('electricmodel')

download.files = F
build.ISD.weather.data(monthly.records = ( 30 * 24) * 0.25, years.of.interest = 2015:2019)
weather.spatial = readRDS('output/ISDweatherstations.RDS')

if (download.files == T) {
  library(doMC)
  registerDoMC(cores=3)
  download.icao.data(weather.spatial, years=2015:2020)
}
# 
# download.demand.data(year.list = 2010:2020) 
# build.data()
# combine.data(smallest.city = 20000)
# 
# build.state.data(select.state = 'NSW')
# build.state.data(select.state = 'VIC')

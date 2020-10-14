devtools::document('electricmodel')

min.year = 2015
max.year = 2020
download.files = F

build.ISD.weather.data(monthly.records = ( 30 * 24) * 0.25, years.of.interest = 2015:2019)
weather.spatial = readRDS('output/ISDweatherstations.RDS')

if (download.files == T) {
  download.demand.data(year.list = min.year:max.year, max.date = '2020-10-01')
  library(doMC)
  registerDoMC(cores=3)
  download.icao.data(weather.spatial, years=min.year:max.year)
  untar.ISD.weather.data(weather.spatial)
  process.ISD.weather.data(weather.spatial)
}

weather.isd.data = readRDS('data/isd-lite/raw.rds')
weather.isd.data = weather.isd.data %>% mutate(id = paste0(station, '-', USAF, '-', WBAN))

station.meta.data = weather.isd.data %>% select(id, station, lat, lon, state, USAF, WBAN) %>% distinct()
weather.isd.data = weather.isd.data %>% select(-station, -lat, -lon, -state, -USAF, -WBAN)

# weather.isd.data = weather.isd.data[1:100000, ]

weather.isd.data = weather.isd.data %>% mutate(ymdh = ymd_hms(paste0(year, '-', month, '-', day, '-', hour, '-00-00')))
weather.isd.data = weather.isd.data %>% select(-year, -month, -day, -hour)
weather.isd.data = padr::pad(weather.isd.data, interval = "hour", start_val=ymd_hms('2015-1-1-0-0-0'), end_val=ymd_hms('2020-9-30-23-0-0'), group='id', break_above = 10)

# This is the NA value
weather.isd.data = na_if(weather.isd.data, -9999)

# These values also look suspicious
weather.isd.data = weather.isd.data %>% mutate(air.temp = na_if(air.temp, 9999), air.temp = na_if(air.temp, 999), 
                            dew.point.temp = na_if(dew.point.temp, 9999), dew.point.temp = na_if(dew.point.temp, 999))

# Now to interpolate between missing hours
weather.isd.data = weather.isd.data %>% group_by(id) %>%
  mutate(dew.point.temp = na.approx(dew.point.temp, maxgap = 6, rule = 2)) # maximum 6 hour gap

# Can't have dew point temperature greater than air tempreature
weather.isd.data = weather.isd.data %>% mutate(dew.point.temp = if_else(dew.point.temp > air.temp, air.temp, dew.point.temp))

# Calculate relative humidity
# https://earthscience.stackexchange.com/questions/16570/how-to-calculate-relative-humidity-from-temperature-dew-point-and-pressure
const.c = 243.04
const.b = 17.625
weather.isd.data = weather.isd.data %>% mutate(dew.point.temp = dew.point.temp/10, air.temp=air.temp/10, relative.humidity = 100*exp((const.c*const.b*(dew.point.temp-air.temp))/((const.c + air.temp)*(const.c + dew.point.temp))))

# drop stations that have gaps
drop.stations = weather.isd.data %>% group_by(id) %>% summarise(nas = sum(is.na(relative.humidity)))

test = weather.isd.data %>% filter(id == 'MELBOURNE INTL-948660-99999')
View(test)

library(worldmet)
dat <- importNOAA(code = "948660-99999", year = 2015)
dat



weather.isd.data = weather.isd.data %>% select(id, ymdh, air.temp, relative.humidity)



weather.isd.data = weather.isd.data %>% left_join(station.meta.data, by='id') 


api.key = "Z2XPTKRSQ5I6K95NCW5L3PDZ4"

https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/history?&aggregateHours=1&startDateTime=2019-06-13T00:00:00&endDateTime=2019-06-20T00:00:00&unitGroup=metric&contentType=csv&dayStartTime=0:0:00&dayEndTime=0:0:00&collectStationContribution&location=Sterling,VA,US&key=Z2XPTKRSQ5I6K95NCW5L3PDZ4
# 
# download.demand.data(year.list = 2010:2020) 
# build.data()
# combine.data(smallest.city = 20000)
# 
# build.state.data(select.state = 'NSW')
# build.state.data(select.state = 'VIC')

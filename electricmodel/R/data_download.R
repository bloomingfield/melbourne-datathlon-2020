

download.demand.data = function(year.list = 2010:2020) {
  base.link = "https://aemo.com.au/aemo/data/nem/priceanddemand/PRICE_AND_DEMAND_YEARMONTH_STATEID.csv"
  
  month.list = c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
  
  state.list = c('NSW1', 'QLD1', 'VIC1', 'SA1', 'TAS1')
  
  grab.grid = expand.grid(month=month.list, year = year.list, state=state.list)
  grab.grid$date = lubridate::ymd(paste0(grab.grid$year, '-', grab.grid$month, '-1'))
  
  grab.grid = grab.grid[grab.grid$date < lubridate::ymd('2020-09-17'), ]
  grab.grid = data.frame(grab.grid)
  
  destination = 'data/aemo/price_demand'
  
  for (i in 1:dim(grab.grid)[1]){
    row = grab.grid[i, ]
    print(row)
    month = row$month
    year = row$year
    state = row$state
    
    try.link = gsub('YEARMONTH', paste0(year, month), base.link, fixed=T)
    try.link = gsub('STATEID', state, try.link, fixed=T)
    download.file(try.link, paste0(destination, '/', state, year, month, '.csv'))
  }
}

# ================================================================================================================================================

download.dispatch.data = function(date.start = ymd('2019-08-21'), date.end = ymd('2020-09-18')) {
  base.link = "http://www.nemweb.com.au/REPORTS/ARCHIVE/Dispatch_SCADA/PUBLIC_DISPATCHSCADA_DATEME.zip"
  
  destination = 'data/aemo/dispatch'
  
  for (i in 0:(date.end-date.start)) {
    date.me = date.start + days(i)
    date.string = paste0(year(date.me),stringr::str_pad(month(date.me), 2, pad='0'), stringr::str_pad(day(date.me), 2, pad='0'))
    try.link = gsub('DATEME', date.string, base.link, fixed=T)
    download.file(try.link, paste0(destination, '/', date.string, '.zip'))
  }
}

# ================================================================================================================================================

download.icao.data = function(weather.spatial, years=2015:2020) {
  base.link = "https://www.ncei.noaa.gov/pub/data/noaa/isd-lite/YEAR/USAF-WBAN-YEAR.gz"
  
  destination = 'data/isd-lite/raw'
  
  for (year in years) {
    foreach(i=1:length(weather.spatial$USAF)) %dopar% {
      row = weather.spatial[i, ]
      print(row)
      USAF = row$USAF
      WBAN = row$WBAN
        
      try.link = gsub('YEAR', year, base.link, fixed=T)
      try.link = gsub('USAF', USAF, try.link, fixed=T)
      try.link = gsub('WBAN', WBAN, try.link, fixed=T)
      download.file(try.link, paste0(destination, '/', USAF, '-', WBAN,'-', year, '.gz'))
    }
  }
}

devtools::document('electricmodel')

download.demand.data(year.list = 2010:2020) 
build.data()
combine.data(smallest.city = 20000)

build.state.data(select.state = 'NSW')
build.state.data(select.state = 'VIC')

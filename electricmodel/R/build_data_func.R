
extract.weather.from.raster = function(locations.use, year=2015, variable='tmin') {
  res = stack(paste0('data/noaa/',variable,'.',year,'.nc'))
  extract.data = res[locations.use$closest.cell]
  result = tibble::rownames_to_column(data.frame((t(extract.data))), "date")
  colnames(result) = c('date', make.names(locations.use$cities.name))
  return(result)
}

extract.solar.from.raster = function(locations.use, year=2015, variable='tmin') {
  res = stack(paste0('data/silo/',year,'.',variable,'.nc'))
  extract.data = res[locations.use$closest.cell]
  result = tibble::rownames_to_column(data.frame((t(extract.data))), "date")
  colnames(result) = c('date', make.names(locations.use$id))
  return(result)
}

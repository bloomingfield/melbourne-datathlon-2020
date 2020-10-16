 
generate.recipe = function(train_data, components = c('date', 'holidays', 'temperature', 'humidity', 'radiation', 'moving.averages')) {
  
  demand_recipe <- recipe(train_data, demand ~ .)
  keep.columns = c()
  keep.columns.starts.with = c()
  
  if ('date' %in% components) {
    # keep.columns = c(keep.columns, 'hour.x', 'hour.y', 'month.x', 'month.y', 'day.of.week', 'is.weekday')
    keep.columns.starts.with = c(keep.columns.starts.with, 'is.weekday')
    keep.columns = c(keep.columns, 'hour', 'month', 'day.of.week', 'is.weekday')
  }  
  if ('date_circular' %in% components) {
    keep.columns = c(keep.columns, 'hour.x', 'hour.y', 'month.x', 'month.y', 'day.of.week', 'is.weekday')
    keep.columns.starts.with = c(keep.columns.starts.with, 'is.weekday')
  }
  if ('holidays' %in% components) {
    keep.columns = c(keep.columns, 'holiday', 'nearest.holiday.distance')
  }
  if ('temperature' %in% components) {
    keep.columns.starts.with = c(keep.columns.starts.with, 'air_temp')
  }
  if ('humidity' %in% components) {
    keep.columns.starts.with = c(keep.columns.starts.with, 'RH')
  }
  if ('radiation' %in% components) {
    keep.columns.starts.with = c(keep.columns.starts.with, 'radiation', 'solar.intensity')
  }
  if ('lags' %in% components) {
    keep.columns.starts.with = c(keep.columns.starts.with, '1daylag_air_temp', '1daylag_radiation', 
                                 '7daylag_air_temp', '7daylag_radiation')
  }
  if ('lags.all' %in% components) {
    keep.columns.starts.with = c(keep.columns.starts.with, '1daylag', '7daylag')
  }
  if ('rolling.average' %in% components) {
    keep.columns.starts.with = c(keep.columns.starts.with, '24hrmean_air_temp', '24hrmean_radiation', 
                                 'weekmean_air_temp', 'weekmean_radiation')
  }
  if ('rolling.average.all' %in% components) {
    keep.columns.starts.with = c(keep.columns.starts.with, '24hrmean', 'weekmean')
  }
  
  demand_recipe = demand_recipe %>%
    step_rm(all_predictors(), -all_of(keep.columns), -starts_with(keep.columns.starts.with)) %>%
    step_normalize(all_numeric(),-all_outcomes()) %>%
    step_dummy(all_nominal(), one_hot = F)
  return(demand_recipe)
}
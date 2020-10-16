 
run.state = function(state.model) {
  dd.list = readRDS(paste0('output/',state.model,'.RDS'))

  # =========================================================
  dd = dd.list$state.data
  dd <- dd %>% rename(demand = TOTALDEMAND) %>% mutate(demand = log(demand))
  # =========================================================
  
  date.split.test = ymd('2019-01-01')
  date.split.predict = ymd('2020-01-01')
  
  test_data <- dd %>% filter(date >= date.split.test & date < date.split.predict)
  train_data <- dd %>% filter(date < date.split.test)
  predict_data <- dd %>% filter(date >= date.split.predict)
  
  full_data = bind_rows(train_data, test_data, predict_data)
  
  components.list = c('date', 'holidays', 'temperature', 'radiation', 'rolling.average')
  components.use = c()
  model.results = list()
  i = 0
  for (components in components.list) {
    i = i + 1
    components.use = c(components.use, components)
    print(components.use)
    demand_recipe = generate.recipe(train_data, components = components.use)
    if (F) {
      test.prep = prep(demand_recipe, new_data = train_data)
      test.bake = bake(test.prep, new_data = train_data)
    }
    res = fit.model(train_data, test_data, predict_data, demand_recipe)
    res$test_fit$.workflow = NULL
    res$predict_fit$.workflow = NULL
    res$xgboost_crossval$.workflow = NULL
    res$test_fit$splits = NULL
    res$predict_fit$splits = NULL
    res$xgboost_crossval$splits = NULL
    
    plot.data = full_data %>% mutate(.row=row_number()) %>% bind_cols()
    plot.data.2020 = left_join(res$predict_fit$.predictions[[1]], plot.data)
    plot.data.2019 = left_join(res$test_fit$.predictions[[1]], plot.data)
    
    model.results[[i]] = list(res=res, components=components.use, plot.data.2019=plot.data.2019, plot.data.2020=plot.data.2020)
  }
  return(model.results)
}
create.rolling.origin.split = function(train_data) {
  furniture_vfold = rolling_origin(train_data %>% nest(-month.year),  initial=24, assess=12, skip=11, cumulative=T)
  
  for (i in 1:dim(furniture_vfold)[1]) { # because rsample is shite
    dd = furniture_vfold$splits[[i]]
    dd$in_id = 1:dim(dd$data[dd$in_id, ] %>% unnest(data))[1]
    dd$out_id = (max(dd$in_id)+1):(max(dd$in_id) + dim(dd$data[dd$out_id, ] %>% unnest(data))[1])
    dd$data = dd$data %>% unnest(data)
    furniture_vfold$splits[[i]] = dd
  }
  return(furniture_vfold)
}

create.train.test.split = function(train_data, test_data) {
  tt_data = bind_rows(train_data, test_data)
  final_split = rsample::initial_split(tt_data)
  final_split$in_id = 1:dim(train_data)[1]
  final_split$out_id = (dim(train_data)[1]+1):dim(tt_data)[1]
  return(final_split)
}



fit.model = function(train_data, test_data, predict_data, recipe, ncores=3, tune=F) {
  
  metric.set = metric_set(rmse, mape)
  
  if (tune) { 
    model = boost_tree(
      trees = tune(), learn_rate = tune(),
      tree_depth = tune(), min_n = tune(),
      loss_reduction = tune(), 
      sample_size = tune(), mtry = tune(), 
    )
  } else {
    model = boost_tree()
  }
  
  xgb_model <- 
    model %>% 
    set_mode("regression") %>% 
    set_engine("xgboost", nthread = ncores)
  
  electricity.workflow <- workflow() %>% 
    add_recipe(recipe) %>%
    add_model(xgb_model)
  
  eletricity.sample <- create.rolling.origin.split(train_data)
  if (tune) {
    xgboost_params <- parameters(
      trees(), learn_rate(),
      tree_depth(), min_n(), 
      loss_reduction(),
      sample_size = sample_prop(), finalize(mtry(), train_data)  
    )
    xgboost_params <- xgboost_params %>% update(trees = trees(c(100, 2000))) 
    
    set.seed(321)
    xgboost_tune <-
      electricity.workflow %>%
      tune_bayes(
        resamples = eletricity.sample,
        param_info = xgboost_params,
        # initial = ?,
        iter = 5, 
        metrics = metric.set,
        control = control_bayes(no_improve = 10, 
                                save_pred = T, verbose = T)
      )
    
    best_model <- select_best(xgboost_tune, "rmse")
    final_model <- finalize_model(xgb_model, best_model)
    final_workflow <- electricity.workflow %>% update_model(final_model)
  } else {
    final_workflow <- electricity.workflow
  }
  
  xgboost_crossval <-
    final_workflow %>%
    fit_resamples(
      resamples = eletricity.sample,
      metrics = metric.set
    )
  
  final_split = create.train.test.split(train_data, test_data)
  test_fit <- last_fit(final_workflow, final_split, metrics = metric.set)
  
  tt_data = bind_rows(train_data, test_data)
  final_split = create.train.test.split(tt_data, predict_data)
  predict_fit <- last_fit(final_workflow, final_split, metrics = metric.set)
  
  return(list(test_fit=test_fit, predict_fit=predict_fit, xgboost_crossval=xgboost_crossval))
}


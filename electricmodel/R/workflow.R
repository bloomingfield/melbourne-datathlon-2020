

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
  
  eletricity.sample <- rolling_origin(train_data, initial = 365*5+366*2, assess=365, skip=364)
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
  
  tt_data = bind_rows(train_data, test_data)
  final_split = rsample::initial_split(tt_data)
  final_split$in_id = 1:dim(train_data)[1]
  final_split$out_id = (dim(train_data)[1]+1):dim(tt_data)[1]
  test_fit <- last_fit(final_workflow, final_split, metrics = metric.set)
  
  ttp_data = bind_rows(tt_data, predict_data)
  final_split = rsample::initial_split(ttp_data)
  final_split$in_id = 1:dim(tt_data)[1]
  final_split$out_id = (dim(tt_data)[1]+1):dim(ttp_data)[1]
  predict_fit <- last_fit(final_workflow, final_split, metrics = metric.set)
  
  return(list(test_fit=test_fit, predict_fit=predict_fit, xgboost_crossval=xgboost_crossval))
}


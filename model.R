
library(visdat)
library(tidyverse)
library(tidymodels)
library(patchwork)

dd = readRDS('output/NSW.RDS')
dd <- dd %>% mutate(demand = log(demand))

date.split = ymd('2020-01-01')

test_data <- dd %>% filter(date >= date.split)
train_data <- dd %>% filter(date < date.split)

vis_dat(train_data)

# hist
g1 <- ggplot(train_data, aes(x=demand)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+ 
  labs(x = "", y = "")

# boxplot
g2 <- ggplot(train_data, aes(y=demand)) + 
  geom_boxplot(aes(x=""), colour="black", fill="white")+
  coord_flip()+ 
  labs(x = "", y = "")

# qqplot
g3 <- ggplot(train_data, aes(sample = demand))+ 
  stat_qq()+
  stat_qq_line()+ 
  labs(x = "", y = "")

g3 | g1 / g2 

demand_recipe <- recipe(train_data, demand ~ date + population+state) %>%
  step_date(date, features = c("dow", "month")) %>%
  step_rm(date, state) %>%
  step_normalize(all_numeric(),-all_outcomes()) %>%
  step_dummy(all_nominal())

test.prep = prep(demand_recipe, new_data = train_data)
test.bake = bake(test.prep, new_data = train_data)

vis_dat(test.bake)

ncores = 3

xgb_model <- 
  boost_tree(
    trees = tune(), learn_rate = tune(),
    tree_depth = tune(), min_n = tune(),
    loss_reduction = tune(), 
    sample_size = tune(), mtry = tune(), 
  ) %>% 
  set_mode("regression") %>% 
  set_engine("xgboost", nthread = ncores)

electricity.workflow <- workflow() %>% 
  add_recipe(demand_recipe) %>%
  add_model(xgb_model)

xgboost_params <- parameters(
  trees(), learn_rate(),
  tree_depth(), min_n(), 
  loss_reduction(),
  sample_size = sample_prop(), finalize(mtry(), train_data)  
)
xgboost_params <- xgboost_params %>% update(trees = trees(c(100, 500))) 

#train_data[nrow(train_data)+1,] <- NA
#train_data[nrow(train_data)+1,] <- NA
eletricity.sample <- rolling_origin(train_data, initial = 366+365, assess = 365, skip = 364)
for (i in dim(eletricity.sample)[1]) {
  eletricity.sample$splits[[i]]$data = eletricity.sample$splits[[i]]$data %>% drop_na()
}
train_data = train_data %>% drop_na()

set.seed(321)
xgboost_tune <-
  electricity.workflow %>%
  tune_bayes(
    resamples = eletricity.sample,
    param_info = xgboost_params,
    # initial = ?,
    iter = 30, 
    metrics = metric_set(rmse, mape),
    control = control_bayes(no_improve = 10, 
                            save_pred = T, verbose = T)
  )

best_model <- select_best(xgboost_tune, "rmse")
final_model <- finalize_model(xgb_model, best_model)
final_workflow    <- electricity.workflow %>% update_model(final_model)


final_split = rsample::initial_split(train_data)
final_split$in_id = eletricity.sample$splits[[3]]$in_id
final_split$out_id = eletricity.sample$splits[[3]]$out_id
final_fit <- last_fit(final_workflow, final_split)

plot.data = train_data %>% mutate(.row=row_number()) %>% bind_cols()
plot.data = left_join(final_fit$.predictions[[1]], plot.data)

g1 <- 
  plot.data %>% 
  ggplot()+
  geom_line(aes(x = date, y = demand, col='actual'))+ 
  geom_line(aes(x = date, y = .pred, col='prediction'))
g1

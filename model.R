devtools::document('electricmodel')

dd = readRDS('output/NSW.RDS')
dd <- dd %>% mutate(demand = demand)

date.split.test = ymd('2019-01-01')
date.split.predict = ymd('2020-01-01')

test_data <- dd %>% filter(date >= date.split.test & date < date.split.predict)
train_data <- dd %>% filter(date < date.split.test)
predict_data <- dd %>% filter(date >= date.split.predict)

vis_dat(train_data)
train_data = train_data %>% replace(is.na(.), 0)

change.in.use.over.time(train_data)
plot.temporal.distributions(train_data, test_data, predict_data)
plot.raw.distributions(train_data)

# hist

demand_recipe <- recipe(train_data, demand ~ .) %>% # date + population+state
  step_date(date, features = c("dow", 'doy', "month", 'year')) %>%
  step_rm(date, state) %>%
  step_normalize(all_numeric(),-all_outcomes()) %>%
  step_dummy(all_nominal())

if (F) {
  test.prep = prep(demand_recipe, new_data = train_data)
  test.bake = bake(test.prep, new_data = train_data)
}

res = fit.model(train_data, test_data, predict_data, demand_recipe)

full_data = bind_rows(train_data, test_data, predict_data)
plot.data = full_data %>% mutate(.row=row_number()) %>% bind_cols()
plot.data = left_join(res$predict_fit$.predictions[[1]], plot.data)

g1 <- 
  plot.data %>% 
  ggplot()+
  geom_line(aes(x = date, y = demand, col='actual')) + 
  geom_line(aes(x = date, y = .pred, col='prediction'))
g1



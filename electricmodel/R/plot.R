
plot.raw.distributions = function(train_data) {
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
}

plot.temporal.distributions = function(train_data, test_data, predict_data) {
  
  train_data = train_data %>% mutate(year= year(date), yday = yday(date))
  test_data = test_data %>% mutate(year= year(date), yday = yday(date), demand.mean = frollmean(demand, 7))
  predict_data = predict_data %>% mutate(year= year(date), yday = yday(date), demand.mean = frollmean(demand, 7))
  agg_data = train_data %>% group_by(yday) %>% summarise(mean=mean(demand), q.95 = quantile(demand, c(0.95)), q.05 = quantile(demand, c(0.05)) )
  
  p = ggplot()
  p = p + geom_line(data=agg_data, aes(x=yday, y=mean, col='2010-2018 mean'), size=2, alpha=0.8)
  p = p + geom_ribbon(data=agg_data, aes(x=yday, ymin=q.05, ymax=q.95), alpha=0.25)
  p = p + geom_line(data=test_data, aes(x=yday, y=demand.mean, col='2019'), size=1, alpha=0.8)
  p = p + geom_line(data=predict_data, aes(x=yday, y=demand.mean, col='2020'), size=1, alpha=0.8)
  p = p + labs(x='Day of year', y='Electricity demand', col='Time period')
  p = p + scale_color_manual(values=c("black", "red", "blue"))
  p
  
}

change.in.use.over.time = function(train_data) {
  agg_data = train_data %>% mutate(month.year=ymd(paste0(year(date),'-',month(date),'-15')))
  agg_data = agg_data %>% group_by(month.year) %>% summarise(demand=sum(demand))
  
  p = ggplot()
  p = p + geom_line(data=agg_data, aes(x=month.year, y=demand))
  p = p + labs(x='Year', y='Eletricity demand')
  p
}





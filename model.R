devtools::document('electricmodel')
source('model_func.R')

if (F) {
  res = run.state('VIC_mod')
  saveRDS(res, file='results/VIC.RDS')
  res = run.state('NSW_mod')
  saveRDS(res, file='results/NSW.RDS')
}

nsw.data = readRDS('results/NSW.RDS')
vic.data = readRDS('results/VIC.RDS')

# ===============================================
# Overlay weather stations over aus population density

nsw.data.orig = readRDS('output/NSW_mod.RDS')
vic.data.orig = readRDS('output/VIC_mod.RDS')
stations = bind_rows(nsw.data.orig$weather.spatial, vic.data.orig$weather.spatial)

library(rgdal)
PG <- readOGR("data/abs/shapefile")    
AG <- fortify(PG)

pop.r = raster('data/abs/popgrid/apg18e_1_0_0.tif')
pop.r = raster::aggregate(pop.r, fact=10,fun=sum)
sr <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 
# Project Raster
projected_raster <- projectRaster(pop.r, crs = sr)
test_spdf <- as(projected_raster, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")

p = ggplot()
p = p + geom_polygon(data = AG, aes(long, lat, group = group), colour="black", fill=NA) 
p = p + geom_tile(data=test_df, aes(x=x, y=y, fill=value), alpha=0.8)
p = p + geom_point(data=stations, aes(x=LON, y=LAT), col='black', alpha=0.8)
p = p + scale_size_area()
p = p +  coord_fixed(xlim=c(140, 155), ylim=c(-40, -28))
p = p + scale_fill_distiller(palette='Spectral')
p = p + labs(x='Longitude', y='Latitude', fill='Population')
print(p)
ggsave('writeup/figures/weather_station_map.png', width=6, height=4)

# ===============================================
# Model performance
data.all.nsw = extract.res.information(nsw.data)
data.all.nsw$state = 'NSW'
data.all.vic = extract.res.information(vic.data)
data.all.vic$state = 'VIC'
data.all = bind_rows(data.all.nsw, data.all.vic)
data.all = data.all %>% filter(.metric=='mape')
p = ggplot()
p = p + geom_point(data=data.all, aes(x=variables, y=.estimate))
p = p + facet_wrap(~state, scales='free_x')
p = p + labs(x='Variable selection', y='Mean absolute percentage error')
p
ggsave('writeup/figures/model_performance.png', width=8, height=4)

# ===============================================
# raw model results
plot.data.2019.nsw = nsw.data[[5]]$plot.data.2019
plot.data.2019.nsw$state = 'NSW'
plot.data.2020.nsw = nsw.data[[5]]$plot.data.2020
plot.data.2020.nsw$state = 'NSW'
plot.data.2019.vic = vic.data[[5]]$plot.data.2019
plot.data.2019.vic$state = 'VIC'
plot.data.2020.vic = vic.data[[5]]$plot.data.2020
plot.data.2020.vic$state = 'VIC'

set.seed(1)
random.dates = plot.data.2019.nsw %>% select(date) %>% distinct() %>% sample_n(18) %>% arrange(date)

g1 <- 
  plot.data.2019.nsw %>% filter(date %in% random.dates$date) %>%
  ggplot()+
  geom_line(aes(x = hour, y = demand, col='actual')) + 
  geom_line(aes(x = hour, y = .pred, col='prediction')) +
  facet_wrap(~date, scales = 'fixed', nrow=3)
g1 = g1 + labs(x='Hour of day', y='Log total demand (NSW)', col='')
g1
ggsave('writeup/figures/model_performance_raw.png', width=12, height=8)

# ===============================================

plot.data.2019 = bind_rows(plot.data.2019.nsw, plot.data.2019.vic)
plot.data.2020 = bind_rows(plot.data.2020.vic, plot.data.2020.nsw)

plot.data.2020 = plot.data.2020 %>% group_by(date, state) %>% summarise(demand = mean(demand), prediction = mean(.pred))
plot.data.2019 = plot.data.2019 %>% group_by(date, state) %>% summarise(demand = mean(demand), prediction = mean(.pred))

# ===============================================

restriction.history = list(
  list(start=ymd_hms('0000-3-20 00:00:00'), end=ymd_hms('0000-5-15 00:00:00'), text='First lockdown', state='NSW'),
  list(start=ymd_hms('0000-3-20 00:00:00'), end=ymd_hms('0000-5-15 00:00:00'), text='First lockdown', state='VIC'),
  list(start=ymd_hms('0000-7-7 00:00:00'), end=ymd_hms('0000-8-2 00:00:00'), text='Melbourne enters stage 3', state='VIC'),
  list(start=ymd_hms('0000-8-2 00:00:00'), end=ymd_hms('0000-10-15 00:00:00'), text='Melbourne enters stage 4', state='VIC')
)
restriction.history = bind_rows(restriction.history)

p.data = bind_rows(plot.data.2019.nsw, plot.data.2019.vic, plot.data.2020.vic, plot.data.2020.nsw)
p.data$year = year(p.data$date)
p.data$week = week(p.data$date)

p.data = p.data %>% group_by(week, state, year) %>% summarise(demand = mean(demand), prediction = mean(.pred), date=min(datetime))
p.data = p.data %>% mutate(difference = prediction-demand)
year(p.data$date) = 0

# ===============================================
p <- 
  ggplot()+
  geom_rect(data= restriction.history, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf, fill=text), alpha=0.25)+
  # geom_hline(yintercept = 0) +
  geom_line(data=p.data, aes(x = date, y = demand, col=paste0(year))) + 
 geom_line(data=p.data %>% filter(year == 2020), aes(x = date, y = prediction, col=paste0(year)), linetype = "dashed") +
  facet_wrap(~state, nrow=2, scales='free')
p = p + labs(x='Time of year', y='Total demand (log weekly average)', fill='Major restrictions', col='Year')
p = p + scale_x_datetime(labels = date_format("%b"))
p
ggsave('writeup/figures/raw_demand.png', width=10, height=6)

# ===============================================
p <- 
  ggplot()+
  geom_rect(data= restriction.history, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf, fill=text), alpha=0.25) +
  geom_hline(yintercept = 0) +
  geom_line(data=p.data, aes(x = date, y = difference, col=paste0(year))) + 
  #  geom_line(data=p.data %>% filter(year == 2020), aes(x = date, y = prediction, col=paste0(year)), linetype = "dashed") +
  facet_wrap(~state, nrow=2, scales='free')
p = p + labs(x='Time of year', y='Difference between prediction and actual demand (log weekly average)', fill='Major restrictions', col='Year')
p = p + scale_x_datetime(labels = date_format("%b"))
p
ggsave('writeup/figures/demand_minus_pred.png', width=10, height=6)

# ===============================================


# ============================================================================

underemployment = load.all.unemployment.data()
underemployment = underemployment %>% filter(date > ymd('2020-1-1'))
underemployment = underemployment %>% mutate(datetime = as_datetime(date))

restriction.history.employment.compare =restriction.history
year(restriction.history.employment.compare$start) = 2020
year(restriction.history.employment.compare$end) = 2020

p = ggplot()
p = p + geom_rect(data= restriction.history.employment.compare, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf, fill=text), alpha=0.25)
p = p + geom_line(data=underemployment, aes(x=datetime, y=underemployment.NSW, col='NSW - underemployment'), linetype='dashed')
p = p + geom_line(data=underemployment, aes(x=datetime, y=unemployment.rate.NSW, col='NSW - unemployed'), linetype='dashed')
p = p + geom_line(data=underemployment, aes(x=datetime, y=underemployment.VIC, col='VIC - underemployment'))
p = p + geom_line(data=underemployment, aes(x=datetime, y=unemployment.rate.VIC, col='VIC - unemployed'))
p = p + scale_color_brewer(type='qual', palette='Set1')
p = p + scale_y_continuous(limits=c(0, NA))
p = p + labs(x='Month of 2020', y='Percentage of labour force', col='State', fill='Major restrictions')
p = p + scale_x_datetime(labels = date_format("%b"), breaks = date_breaks("1 months"))
p
ggsave('writeup/figures/unemployment.png', width=6, height=4)









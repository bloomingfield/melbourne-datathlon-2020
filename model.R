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
p = p + labs(x='Longitude', y='Latitude', col='Population')
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
p = p + facet_wrap(~state, scales='free')
p = p + labs(x='variable selection', y='Mean aboslute percentage error')
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

g1 <- 
  plot.data.2019.nsw %>% filter(month.year == '2019-3') %>%
  ggplot()+
  geom_line(aes(x = hour, y = demand, col='actual')) + 
  geom_line(aes(x = hour, y = .pred, col='prediction')) +
  facet_wrap(~date, scales = 'free', nrow=3)
g1 = g1 + labs(x='Hour of day', y='Log total demand (NSW)', col='')
g1
ggsave('writeup/figures/model_performance_raw.png', width=12, height=8)

# ===============================================

plot.data.2019 = bind_rows(plot.data.2019.nsw, plot.data.2019.vic)
plot.data.2020 = bind_rows(plot.data.2020.vic, plot.data.2020.nsw)

plot.data.2020 = plot.data.2020 %>% group_by(date, state) %>% summarise(demand = mean(demand), prediction = mean(.pred))
plot.data.2019 = plot.data.2019 %>% group_by(date, state) %>% summarise(demand = mean(demand), prediction = mean(.pred))

year(plot.data.2020$date) = 0
year(plot.data.2019$date) = 0

g1 <- 
  ggplot()+
  geom_line(data=plot.data.2020, aes(x = date, y = demand, col='2020'))+
  # geom_line(data=plot.data.2020, aes(x=date, y=rollmean(demand, 7, na.pad=TRUE), col='2020')) +
  geom_line(data=plot.data.2019, aes(x = date, y = demand, col='2019'))+
  # geom_line(data=plot.data.2019, aes(x=date, y=rollmean(demand, 7, na.pad=TRUE), col='2019')) +
  facet_wrap(~state, nrow=2)
g1

year(plot.data.2019.nsw$datetime) = 0
year(plot.data.2020.nsw$datetime) = 0
year(plot.data.2019.vic$datetime) = 0
year(plot.data.2020.vic$datetime) = 0

# ============================================================================


restriction.history = list(
  list(start=ymd_hms('0000-3-20 00:00:00'), end=ymd_hms('0000-5-15 00:00:00'), text='First lockdown', state='NSW'),
  list(start=ymd_hms('0000-3-20 00:00:00'), end=ymd_hms('0000-5-15 00:00:00'), text='First lockdown', state='VIC'),
  list(start=ymd_hms('0000-7-7 00:00:00'), end=ymd_hms('0000-8-2 00:00:00'), text='Melbourne enters stage 3', state='VIC'),
  list(start=ymd_hms('0000-8-2 00:00:00'), end=ymd_hms('0000-10-15 00:00:00'), text='Melbourne enters stage 4', state='VIC')
)
restriction.history = bind_rows(restriction.history)

p = ggplot()
# p = p + geom_line(data=plot.data.2019.vic, aes(x = day.of.year, y = demand, col='2019'))
# p = p + geom_line(data=plot.data.2019.nsw, aes(x=datetime, y=rollmean(.pred, 7*48, na.pad=TRUE), col='2019-model'))
p = p + geom_line(data=plot.data.2019.nsw, aes(x=datetime, y=rollmean(demand, 7*48, na.pad=TRUE, align='right'), col='2019'))
p = p + geom_line(data=plot.data.2019.vic, aes(x=datetime, y=rollmean(demand, 7*48, na.pad=TRUE, align='right'), col='2019'))
# p = p + geom_line(data=plot.data.2020.nsw, aes(x=datetime, y=rollmean(.pred, 7*48, na.pad=TRUE), col='2020-model'))
p = p + geom_line(data=plot.data.2020.nsw, aes(x=datetime, y=rollmean(demand, 7*48, na.pad=TRUE, align='right'), col='2020'))
p = p + geom_line(data=plot.data.2020.vic, aes(x=datetime, y=rollmean(demand, 7*48, na.pad=TRUE, align='right'), col='2020'))

p = p + geom_rect(data= restriction.history, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf, fill=text), alpha=0.25)

p = p + facet_wrap(~state, scales='free', nrow=2)
p = p + scale_color_brewer(type='qual', palette='Set1')
p = p + labs(x='Time of year', y='Total demand (log weekly rolling average)', col='Year')
# p = p + geom_line(data=plot.data.2020.vic, aes(x = day.of.year, y = demand, col='2020'))
p
ggsave('writeup/figures/raw_demand.png', width=10, height=6)

# ==============================================

p = ggplot()
# p = p + geom_line(data=plot.data.2019.vic, aes(x = day.of.year, y = demand, col='2019'))
# p = p + geom_line(data=plot.data.2019.nsw, aes(x=datetime, y=rollmean(.pred, 7*48, na.pad=TRUE), col='2019-model'))
p = p + geom_line(data=plot.data.2019.nsw, aes(x=datetime, y=rollmean(.pred-demand, 7*48, na.pad=TRUE, align='right'), col='2019'))
p = p + geom_line(data=plot.data.2019.vic, aes(x=datetime, y=rollmean(.pred-demand, 7*48, na.pad=TRUE, align='right'), col='2019'))
# p = p + geom_line(data=plot.data.2020.nsw, aes(x=datetime, y=rollmean(.pred, 7*48, na.pad=TRUE), col='2020-model'))
p = p + geom_line(data=plot.data.2020.nsw, aes(x=datetime, y=rollmean(.pred-demand, 7*48, na.pad=TRUE, align='right'), col='2020'))
p = p + geom_line(data=plot.data.2020.vic, aes(x=datetime, y=rollmean(.pred-demand, 7*48, na.pad=TRUE, align='right'), col='2020'))
p = p + geom_rect(data= restriction.history, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf, fill=text), alpha=0.25)
p = p + facet_wrap(~state, scales='free', nrow=2)
p = p + scale_color_brewer(type='qual', palette='Set1')
p = p + labs(x='Time of year', y='Difference between predicted and actual demand (log weekly rolling average)', col='Year')
# p = p + geom_line(data=plot.data.2020.vic, aes(x = day.of.year, y = demand, col='2020'))
p
ggsave('writeup/figures/demand_minus_pred.png', width=10, height=6)

# ============================================================================

underemployment = load.all.unemployment.data()
underemployment = underemployment %>% filter(date > ymd('2020-1-1'))
underemployment = underemployment %>% mutate(datetime = as_datetime(date))

restriction.history.employment.compare =restriction.history
year(restriction.history.employment.compare$start) = 2020
year(restriction.history.employment.compare$end) = 2020

p = ggplot()
p = p + geom_line(data=underemployment, aes(x=datetime, y=underemployment.NSW/100, col='NSW - underemployment'))
p = p + geom_line(data=underemployment, aes(x=datetime, y=unemployment.rate.NSW/100, col='NSW - unemployed'))
p = p + geom_line(data=underemployment, aes(x=datetime, y=underemployment.VIC/100, col='VIC - underemployment'))
p = p + geom_line(data=underemployment, aes(x=datetime, y=unemployment.rate.VIC/100, col='VIC - unemployed'))
p = p + scale_color_brewer(type='qual', palette='Paired')
p = p + geom_rect(data= restriction.history.employment.compare, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf, fill=text), alpha=0.25)
p = p + scale_y_continuous(limits=c(0, NA))
p
ggsave('writeup/figures/unemployment.png', width=6, height=4)









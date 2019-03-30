library(dplyr)
library(plotly)
setwd('~/code/MAE399')
source('./readData.r')

d <- readCSVDirectory('./data')
stimes <- extractDate(d$starttime, c('wday', 'hour'))
etimes <- extractDate(d$stoptime, c('wday', 'hour'))
d$start.wday <- stimes$wday
d$end.wday <- etimes$wday
d$start.hour <- stimes$hour
d$end.hour <- etimes$hour
attach(d)

spos <- group_by(d[start.wday >= 2 & start.wday <= 6 & start.hour >= 6 & start.hour <= 10, ], start.station.name[start.wday >= 2 & start.wday <= 6 & start.hour >= 6 & start.hour <= 10])
spos <- spos %>% summarize (freq=n(), lat=last(start.station.latitude), long=last(start.station.longitude))
epos <- group_by(d[end.wday >= 2 & end.wday <= 6 & end.hour >= 6 & end.hour <= 10, ], end.station.name[end.wday >= 2 & end.wday <= 6 & end.hour >= 6 & end.hour <= 10])
epos <- epos %>% summarize (freq=n(), lat=last(end.station.latitude), long=last(end.station.longitude))

plot_ly(spos, x = ~long, y = ~lat, color = ~freq, size = ~freq, asp = 1)
plot_ly(epos, x = ~long, y = ~lat, color = ~freq, size = ~freq, asp = 1)
# Weather Canada
# Sebastien Portalier
# 3 March 2020

library(weathercan)

setwd("~/Documents/Budworm_models/Weather_data")

sta=stations

im=which(lon==max(lon,na.rm=T))
sta$lon[im]=-66.5

plot(sta$lat~sta$lon,pch=20)

pdf("Stations.pdf", height = 7,width = 12)
par(mar=c(5,5,4,2))
plot(sta$lat~sta$lon,pch=20,xlab='Longitude',ylab='Latitude',cex.lab=2)
dev.off()

montreal1=weather_dl(station_ids = 10873,start='2000-01-01',end='2019-12-31',interval = 'day')

tim=seq(1,nrow(montreal1))/365+2000

pdf("Montreal.pdf", height = 7,width = 12)
par(mar=c(5,5,4,2))
plot(montreal1$mean_temp~tim,type='l',lwd=2,xlab='Year',ylab='Temperature in C',cex.lab=1.6)
dev.off()

montreal1=weather_dl(station_ids = 10873,start='2004-01-01',end='2004-12-31',interval = 'hour')
tim=seq(1,nrow(montreal1))/24
pdf("Montreal_Hours.pdf", height = 7,width = 12)
par(mar=c(5,5,4,2))
plot(montreal1$temp~tim,type='l',lwd=2,xlab='Day in 2004',ylab='Temperature in C',cex.lab=1.6)
dev.off()

stations_search(name = 'Edmonton')
edmonton=weather_dl(station_ids = 27214,start='2000-01-01',end='2019-12-31',interval = 'day')
tim=seq(1,nrow(edmonton))/365+2000

pdf("Edmonton_Day.pdf", height = 7,width = 12)
par(mar=c(5,5,4,2))
plot(edmonton$mean_temp~tim,type='l',lwd=2,xlab='Year',ylab='Temperature in C',cex.lab=1.6)
dev.off()

edmonton=weather_dl(station_ids = 27214,start='2009-01-01',end='2010-12-31',interval = 'hour')
tim=seq(1,nrow(edmonton))/24

pdf("Edmonton_Hours.pdf", height = 7,width = 12)
par(mar=c(5,5,4,2))
plot(edmonton$temp~tim,type='l',lwd=2,xlab='Day in 2009-2010',ylab='Temperature in C',cex.lab=1.6)
dev.off()

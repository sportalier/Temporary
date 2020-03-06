# Analysis of data on functional response
# Sebastien Portalier
# 4 March 2020

#### Standard error ####
se=function(dat){
  ste=sd(dat)/sqrt(length(dat))
  return(ste)
}

#### Data ####
setwd("~/Documents/Temporary/Functional_Response")

fr=read.csv('FR.csv')

levels(fr$published_data.habitat)

fresh=fr[fr$published_data.habitat=='freshwater',]
marin=fr[fr$published_data.habitat=='marine',]
terre=fr[fr$published_data.habitat=='terrestrial',]

water1=subset(fr,fr$published_data.habitat!='terrestrial')
water=subset(water1,water1$published_data.habitat!='')
water=droplevels(water)

predator=water$published_data.interactor1sizesi
prey=water$published_data.interactor2sizesi

plot(log10(prey)~log10(predator),xlim=c(-7.5,-2),ylim=c(-7.5,-2))

levels(water$published_data.interactor1)
ranatra=subset(water,water$published_data.interactor1=='Ranatra dispar')
ranatra=droplevels(ranatra)
levels(ranatra$published_data.interactor2)

cons=ranatra$published_data.standardisedtraitvalue
abund=ranatra$published_data.interactor2denvaluesi

plot(log10(cons)~log10(abund))
avercons=tapply(ranatra$published_data.standardisedtraitvalue,as.factor(ranatra$published_data.interactor2denvaluesi),mean)
secons=tapply(ranatra$published_data.standardisedtraitvalue,as.factor(ranatra$published_data.interactor2denvaluesi),se)

resu=matrix(nrow=length(avercons),ncol=4)
resu=as.data.frame(resu)
resu[,1]=as.numeric(names(avercons))
resu[,2]=avercons
resu[,3]=resu[,2]-1.96*secons
resu[,4]=resu[,2]+1.96*secons

plot(resu[,2]~resu[,1],pch=20,ylim=c(1e-5,3e-4))
arrows(resu[,1],resu[,3],resu[,1],resu[,4],length=0.25,angle=90,code=3)

#### water ####
setwd("~/Documents/Temporary/Functional_Response")

fr=read.csv('FR.csv')
water1=subset(fr,fr$published_data.habitat!='terrestrial')
water=subset(water1,water1$published_data.habitat!='')
water=droplevels(water)

predator=water$published_data.interactor1sizesi
prey=water$published_data.interactor2sizesi

pred=levels(water$published_data.interactor1)

inter=subset(water,water$published_data.interactor1==pred[1])
inter=droplevels(inter)
levels(inter$published_data.interactor2)
levels(as.factor(inter$published_data.interactor1sizesi))
levels(as.factor(inter$published_data.interactor2sizesi))

dodo=water[,c(2,17,51,85,86,87,88,89,95,96,97,128,129,130,131,132,138,139)]

#### terrestrial ####
terre=subset(fr,fr$published_data.habitat=='terrestrial')
terre=droplevels(terre)
dodoterre=terre[,c(2,17,51,85,86,87,88,89,95,96,97,128,129,130,131,132,138,139)]

levels(dodoterre$published_data.interactor1)


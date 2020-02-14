# Bipartite network
# 10 February 2020
# Llopis-Belenguer et al., 2020, Echography

setwd("~/Documents/Network")

azov=read.csv("Azov.csv")
japan=read.csv("Japan.csv")

az=azov[,-c(1,2,3,4,5,6,7,8,9,11,12,13,14,15)]
az=azov[,-c(1:15)]
rownames(az)=azov[,1]
az=as.matrix(az)

plotweb(az)
visweb(az)
modaz=computeModules(az)
plotModuleWeb(modaz)

modules=modaz@modules[-1,-c(1:2)]

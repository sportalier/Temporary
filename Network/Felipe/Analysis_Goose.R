# Goose exploration
# Sebastien Portalier
# 11 February 2020

#### network exploration ####
library(bipartite)

#setwd("~/Documents/Network/Felipe")
setwd("~/Documents/Temporary/Network/Felipe")

dat=read.csv("Goose_Data.csv")

goose=dat[,-c(1:5)]
goose=as.matrix(goose)
rownames(goose)=dat[,1]

plotweb(goose,text.rot = 90,col.low = 'green',col.high = 'blue')

goose2=t(goose)
plotweb(goose2)
visweb(goose2)
plotPAC(goose2,scaling=0.4)

visweb(goose2[,1:50])

png("Goose_Network.png",width=2000,height=600)
plotweb(goose2)
dev.off()

png("Goose_Network2.png",width=2000,height=600)
plotweb(goose,text.rot = 90,col.low = 'green',col.high = 'blue')
dev.off()


gplot(as.one.mode(goose,project = 'higher'),gmode = 'graph',label = colnames(goose),label.cex = 0.6,vertex.cex = 2)
#gplot(as.one.mode(goose2,project = 'higher'),gmode = 'graph',label = rownames(goose2),label.cex = 0.6,vertex.cex = 2)

networklevel(goose,index = 'connectance')
networklevel(goose,index = 'number of compartments')
networklevel(goose,index = 'nestedness')
networklevel(goose,index = 'weighted nestedness')
networklevel(goose,index = 'linkage density')
networklevel(goose,index = 'Shannon diversity')

modgo=computeModules(goose)
#plotModuleWeb(modgo)

modules=modgo@modules[-1,-c(1:2)]
listModuleInformation(modgo)

#### non zero ####
library(bipartite)

setwd("~/Documents/Temporary/Network/Felipe")

dat=read.csv("Goose_Data.csv")

goose2=dat[,-c(1:5)]
goose2=as.matrix(goose2)
rownames(goose2)=dat[,1]
totpar=apply(goose2,1,sum)
totparindex=which(totpar!=0)

goose=goose2[totparindex,]

plotweb(goose,text.rot = 90,col.low = 'green',col.high = 'blue')

modgo=computeModules(goose,method = 'DormannStrauss')
modules=modgo@modules[-1,-c(1:2)]

ro=rownames(goose)
co=colnames(goose)
nam=c(ro,co)

g1=nam[which(modules[1,]!=0)]
g2=nam[which(modules[2,]!=0)]
g3=nam[which(modules[3,]!=0)]
g4=nam[which(modules[4,]!=0)]
g5=nam[which(modules[5,]!=0)]


# g1: HD
# g2: DL, DB, CA, SG, ER, CL
# g3: TT, PC

g1h=as.numeric(g1[-232])
g2h=as.numeric(g2[-c(58:63)])
g3h=as.numeric(g3[-c(210,211)])

modgo=metaComputeModules(goose,N=10)
modules=modgo@modules[-1,-c(1:2)]

ro=rownames(goose)
co=colnames(goose)
nam=c(ro,co)

g1=nam[which(modules[1,]!=0)]
g2=nam[which(modules[2,]!=0)]
g3=nam[which(modules[3,]!=0)]
g4=nam[which(modules[4,]!=0)]

tail(g1)
tail(g2)
tail(g3)
tail(g4)

# 1: TT, PC
# 2: HD
# 3: DL, DB, SG, CL
# 4: CA, ER

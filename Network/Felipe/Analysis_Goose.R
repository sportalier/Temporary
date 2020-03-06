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

modgo=metaComputeModules(goose,N=1000)
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

#### bootstrap ####
nsize=nrow(goose)
id=seq(1,nsize)
res

for (i in 1:1000){
  sam=sample(id,nsize,replace=T)
  gooseboot=goose[id,]
  rownames(gooseboot)=id
  modgo=metaComputeModules(gooseboot,N=10)
  modules=modgo@modules[-1,-c(1:2)]
  ro=rownames(gooseboot)
  co=colnames(gooseboot)
  nam=c(ro,co)
  g1=nam[which(modules[1,]!=0)]
  g2=nam[which(modules[2,]!=0)]
  g3=nam[which(modules[3,]!=0)]
  g4=nam[which(modules[4,]!=0)]
  t1=tail(g1)
  t2=tail(g2)
  t3=tail(g3)
  t4=tail(g4)
  
}

write.csv(modules,'Modules.csv', quote=F, row.names = F)

#### module attribution ####
setwd("~/Documents/Temporary/Network/Felipe")
dat=read.csv("Goose_Data.csv")
goose2=dat[,-c(1:5)]
goose2=as.matrix(goose2)
rownames(goose2)=dat[,1]
totpar=apply(goose2,1,sum)
totparindex=which(totpar!=0)
goose=goose2[totparindex,]
total=dat[totparindex,]
rm(dat,goose2)
modules=read.csv('Modules.csv')

ro=rownames(goose)
co=colnames(goose)
nam=c(ro,co)
g1=nam[which(modules[1,]!=0)]
g2=nam[which(modules[2,]!=0)]
g3=nam[which(modules[3,]!=0)]
g4=nam[which(modules[4,]!=0)]
t1=tail(g1)
t2=tail(g2)
t3=tail(g3)
t4=tail(g4)
# t1: DL, DB, SG, CL
# t2: TT, PC
# t3: HD
# t4: CA, ER

Module=rep(0,nrow(total))
modo=modules[,-c(599:607)]
mod1=which(modo[1,]!=0)
mod2=which(modo[2,]!=0)
mod3=which(modo[3,]!=0)
mod4=which(modo[4,]!=0)

Module[mod1]='G1'
Module[mod2]='G2'
Module[mod3]='G3'
Module[mod4]='G4'

total=cbind(total,Module)
write.csv(total,'Total.csv',quote=F,row.names = F)

#### module analysis ####
setwd("~/Documents/Temporary/Network/Felipe")
total=read.csv('Total.csv')

table(total$SEX,total$Module)
chisq.test(total$SEX,total$Module)

table(total$AGE,total$Module)
chisq.test(total$AGE,total$Module)

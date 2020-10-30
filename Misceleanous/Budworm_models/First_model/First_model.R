##
# Model with one larval stage, reproduction and mortality #
##
# by Sebastien
# 11 December 2019
##

library(deSolve)

#### parameters ####
m1 = 6e-5 # natural death
m2 = 4e-5 # intra-specific competition

b = 50 # reproduction rate

#### development rate ####
beta1 = 0.194
beta2 = 3
beta3 = 5.84
beta4 = 0.034
tb = 2.5
tm = 35

devel = function(temp){
  if (temp<=tm && temp>=tb){
    tau=(temp-tb)/(tm-tb)
    rtemp=beta1*(1/(1+exp(beta2-beta3*tau))-exp((tau-1)/beta4))
  }else{
    rtemp=0
  }
  if (rtemp<0) rtemp=0
  return(rtemp)
}

# population decay during Ln
decay = function(t,x,parms){
  m1=parms[1]
  m2=parms[2]
  y=-m1*x-m2*x^2
  return(list(y))
}

# #test
# temperature=seq(0,40,by=0.2)
# rt=rep(0,length(temperature))
# for (i in 1:length(rt)){
#   rt[i]=devel(temperature[i])
# }
# 
# plot(rt~temperature,type='l',lwd=2)

#### temperature variation ####
Mt = 8 # mean temperature
At = 28 # amplitude

year = seq(1,365)
yeartemperature = Mt-At*cos(2*pi*year/365)

#plot(temperature~year,type='l',lwd=2)

#### initial conditions ####
ln0=2000
st = 270 # spawning time mid August
lifestagenumber=7 # number of life stages for a complete cycle
totalgeneration=100
currentyear=1
timemod=0

#### model ####
Lnpop=matrix(0,nrow=totalgeneration,ncol=3)
Lnpop=as.data.frame(Lnpop)
Lnpop[1,1]=ln0
Lnpop[1,2]=st
Lnpop[1,3]=currentyear


for (curgeneration in 2:totalgeneration){
  # the different life stages
  for (currentstage in 1:lifestagenumber){
    # duration of Ln
    lifecycletemp=c(yeartemperature[st:365],yeartemperature[1:(st-1)]) # temperature during life cycle
    devLn=rep(0,365)
    for (i in 1:365){
      devLn[i]=devel(lifecycletemp[i])
    }
    cumuldevLn=cumsum(devLn)
    #plot(cumuldevLn~year,type='l',lwd=2)
    #abline(a=1,b=0,col='red',lwd=2)
    timeLn=which(cumuldevLn>=1.0)[[1]]
    
    # population decay during Ln
    tvoulu=seq(1,timeLn)
    
    sol=lsoda(ln0,tvoulu,decay,parms=c(m1,m2),rtol=1e-9,atol=1e-9)
    ln0=sol[timeLn,2][[1]]
    st=timeLn+st
    if (st>365){
      st=st-365
    }
    # real time
    timemod=timemod+timeLn
    if (timemod>365){
      timemod=timemod-365
      currentyear=currentyear+1
    }
  }
  
  # reproduction
  ln0=ln0*b
  
  # result storage
  Lnpop[curgeneration,1]=ln0
  Lnpop[curgeneration,2]=st
  Lnpop[curgeneration,3]=currentyear
}

plot(Lnpop[-1,1]~seq(1,(nrow(Lnpop)-1)),type='l',lwd=2,xlab="# generations",ylab="Population density")
plot(Lnpop[,2]~seq(1,nrow(Lnpop)),type='l',lwd=2,xlab="# generations",ylab="Spawning time (Julian day)")
plot(Lnpop[,3]~seq(1,nrow(Lnpop)),type='l',lwd=2,xlab="# generations",ylab="# years")

numyears=Lnpop[nrow(Lnpop),3]
numgen=matrix(0,nrow=numyears,ncol=2)
numgen=as.data.frame(numgen)
currentyear=1
localnumgen=0
for (i in 1:nrow(Lnpop)){
  if (Lnpop[i,3]==currentyear){
    localnumgen=localnumgen+1
  }else{
    numgen[currentyear,1]=currentyear
    numgen[currentyear,2]=localnumgen
    currentyear=currentyear+1
    localnumgen=1
  }
}

plot(numgen[-numyears,2]~numgen[-numyears,1],pch=6,xlab='Time (year)',ylab='# generations per year')
lines(numgen[-numyears,2]~numgen[-numyears,1],lwd=1,col='blue')

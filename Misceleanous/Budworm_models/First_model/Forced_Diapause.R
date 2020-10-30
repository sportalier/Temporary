##
# Simple model with forced diapause #
##
# by Sebastien
# 17 December 2019
##

library(deSolve)

#### parameters ####
m1 = 6e-5 # natural death
m2 = 4e-5 # intra-specific competition

b = 50 # reproduction rate

#### development rate ####
beta1 = c(0.228,0.277,0.194,0.919,0.438,1.211,0.269,0.317,0.205,57.8)
beta2 = c(3.12,32.14,3.0,2.91,3.06,3.8,3.02,3.06,2.85,-3.08)
beta3 = c(5.94,11.63,5.84,5.32,6.85,7.55,8.57,4.66,6.28,0.045)
beta4 = c(0.073,0,0.034,0.061,0.061,0.148,0.005,0.136,0.044,0)
tb = c(6,6.2,2.5,4.4,4.4,4.4,4.4,4.4,4.4,8)
tm = c(35,0,35,38,38,38,38,38,35,35)

paramdev = cbind(beta1,beta2,beta3,beta4,tb,tm)

devel = function(temp,parms){
  beta1=parms[1]
  beta2=parms[2]
  beta3=parms[3]
  beta4=parms[4]
  tb=parms[5]
  tm=parms[6]
  if (temp<=tm && temp>=tb){
    tau=(temp-tb)/(tm-tb)
    rtemp=beta1*(1/(1+exp(beta2-beta3*tau))-exp((tau-1)/beta4))
  }else{
    rtemp=0
  }
  if (rtemp<0) rtemp=0
  return(rtemp)
}

devel2 = function(temp,parms){
  beta1=parms[1]
  beta2=parms[2]
  beta3=parms[3]
  beta4=parms[4]
  tb=parms[5]
  tm=parms[6]
  if (temp>=tb){
    rtemp=beta1*exp(-0.5*((temp-beta2)/beta3)^2)
  }else{
    rtemp=0
  }
  if (rtemp<0) rtemp=0
  return(rtemp)
}

devel3 = function(temp,parms){
  beta1=parms[1]
  beta2=parms[2]
  beta3=parms[3]
  beta4=parms[4]
  tb=parms[5]
  tm=parms[6]
  tau=min(max(temp,tb),tm)
  rtemp=1/(beta1+beta2*tau+beta3*tau^3)
  return(rtemp)
}

# population decay during Ln
decay = function(t,x,parms){
  m1=parms[1]
  m2=parms[2]
  y=-m1*x-m2*x^2
  return(list(y))
}

#### temperature variation ####
Mt = 8 # mean temperature
At = 28 # amplitude

year = seq(1,365)
yeartemperature = Mt-At*cos(2*pi*year/365)

#### initial conditions ####
ln0=2000
st = 270 # spawning time mid August
lifestagenumber=length(beta1) # number of life stages for a complete cycle
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
    lifecycletemp=c(yeartemperature[st:365],yeartemperature[1:(st-1)]) # temperature during life stage
    devLn=rep(0,365)
    parms=paramdev[currentstage,]
    if (currentstage == 2){
      for (i in 1:365){
        devLn[i]=devel2(lifecycletemp[i],parms)
      }
    }else{
      if (currentstage == lifestagenumber){
        for (i in 1:365){
          devLn[i]=devel3(lifecycletemp[i],parms)
        }
      }else{
        for (i in 1:365){
          devLn[i]=devel(lifecycletemp[i],parms)
        }
      }
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

plot(Lnpop[-1,1]~seq(1,(nrow(Lnpop)-1)),type='l',lwd=2,xlab="# generations",ylab="Population density")
plot(Lnpop[,2]~seq(1,nrow(Lnpop)),type='l',lwd=2,xlab="# generations",ylab="Spawning time (Julian day)")
plot(Lnpop[,3]~seq(1,nrow(Lnpop)),type='l',lwd=2,xlab="# generations",ylab="# years")

plot(numgen[-numyears,2]~numgen[-numyears,1],pch=6,xlab='Time (year)',ylab='# generations per year')
lines(numgen[-numyears,2]~numgen[-numyears,1],lwd=1,col='blue')



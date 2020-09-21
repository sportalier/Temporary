# Taylor et al. 2013
# 24 February 2020

library(deSolve)

r=0.5
K=30000

growth=function(t,x,parms){
  #y=r*x*(1-x/K)
  y=r*x-x^2*r/K
  return(list(y))
}

tvoulu=seq(1,1e4)
x0=20

res=lsoda(x0,tvoulu,growth,rtol=1e-9)

plot(res[,2]~log10(res[,1]),type='l')

growth2=function(t,x,parms){
  y=r*x-r*x^2
  return(list(y))
}

tvoulu=seq(1,1e4)
x0=20

res=lsoda(x0,tvoulu,growth2,rtol=1e-9)

e=1.2
year=seq(1,365)
season=r*(1-e*sin(2*pi*year))

plot(season~year,type='l')

e=0.2
growth3=function(t,x,parms){
  y=r*(1-e*sin(2*pi*t))*x-(x^2)*r/K
  return(list(y))
}

res2=lsoda(x0,tvoulu,growth3,rtol=1e-9)

plot(res2[,2]~log10(res[,1]),type='l',col='red')
lines(res[,2]~log10(res[,1]))

l=1.5
e=1.2
year=seq(1,365)
season=r*(1-e*sin(2*pi*year))^l

plot(season~year,type='l')

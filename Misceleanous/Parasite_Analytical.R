######
## Parasite analytical
## by Sebastien
## 13th January 2020
######

library(rootSolve)

#### ODEs ####
model=function(t,x,parms){
  a1=parms[1]
  b1=parms[2]
  alpha1=parms[3]
  upsilon1=parms[4]
  gamma1=parms[5]
  beta11=parms[6]
  beta21=parms[7]
  a2=parms[8]
  b2=parms[9]
  alpha2=parms[10]
  upsilon2=parms[11]
  gamma2=parms[12]
  beta22=parms[13]
  beta12=parms[14]
  K1=parms[15]
  K2=parms[16]
  
  y=rep(0,6)
  Ntot=x[1]+x[2]+x[3]+x[4]+x[5]+x[6]
  y[1]=a1*(x[1]+x[2]+x[3])*(1-(x[1]+x[2]+x[3])/K1)+gamma1*x[3]-beta11*x[1]*x[2]/Ntot-beta21*x[1]*x[5]/Ntot
  y[2]=beta11*x[1]*x[2]/Ntot+beta21*x[1]*x[5]/Ntot-alpha1*x[2]-b1*x[2]-upsilon1*x[2]
  y[3]=upsilon1*x[2]-gamma1*x[3]
  y[4]=a2*(x[4]+x[5]+x[6])*(1-(x[4]+x[5]+x[6])/K2)+gamma2*x[6]-beta22*x[4]*x[5]/Ntot-beta21*x[4]*x[2]/Ntot
  y[5]=beta22*x[4]*x[5]/Ntot+beta21*x[4]*x[2]/Ntot-alpha2*x[5]-b2*x[5]-upsilon2*x[5]
  y[6]=upsilon2*x[5]-gamma2*x[6]
  return(list(y))
}

#### Parameters ####
a1=5
a2=5
b1=0.2
b2=0.2
alpha1=6
alpha2=4
upsilon1=4
upsilon2=4
gamma1=.4
gamma2=.6
beta11=4
beta21=0.6
beta22=6
beta12=0.8
K1=80000
K2=60000

parms=c(a1,b1,alpha1,upsilon1,gamma1,beta11,beta21,a2,b2,alpha2,upsilon2,gamma2,beta22,beta12,K1,K2)

#### Initial values ####
x0=c(600,600,600,600,600,600)

#### Roots ####
RS <- runsteady(y = x0, fun = model,parms = parms, times = c(0, 1e5))

y=RS$y
S1=ifelse(y[1]>=0,y[1],0)
I1=ifelse(y[2]>=0,y[2],0)
R1=ifelse(y[3]>=0,y[3],0)
S2=ifelse(y[4]>=0,y[4],0)
I2=ifelse(y[5]>=0,y[5],0)
R2=ifelse(y[6]>=0,y[6],0)

y=RS$y
S1=y[1]
I1=y[2]
R1=y[3]
S2=y[4]
I2=y[5]
R2=y[6]

#### Solve ####
# library(deSolve)
# 
# tvoulu=seq(1,1e5)
# res=lsoda(x0,tvoulu,model,parms,rtol=1e-9,atol=1e-9,maxsteps=1e5)


#### Jacobian matrix ####
jacob = matrix(nrow = 6, ncol = 6)

# df(S1)/dS1
jacob[1,1]=-(-I2*S1*beta21-I1*S1*beta11)/(S2+S1+R2+R1+I2+I1)^2+(-I2*beta21-I1*beta11)/(S2+S1+R2+R1+I2+I1)+(S1*a1+R1*a1+I1*a1)/K1+((S1+R1+I1)/K1+1)*a1
# df(S1)/dI1
jacob[1,2]=-(-I2*S1*beta21-I1*S1*beta11)/(S2+S1+R2+R1+I2+I1)^2-(S1*beta11)/(S2+S1+R2+R1+I2+I1)+(S1*a1+R1*a1+I1*a1)/K1+((S1+R1+I1)/K1+1)*a1
# df(S1)/dR1
jacob[1,3]=gamma1-(-I2*S1*beta21-I1*S1*beta11)/(S2+S1+R2+R1+I2+I1)^2+(S1*a1+R1*a1+I1*a1)/K1+((S1+R1+I1)/K1+1)*a1
# df(S1)/dS2
jacob[1,4]=-(-I2*S1*beta21-I1*S1*beta11)/(S2+S1+R2+R1+I2+I1)^2
# df(S1)/dI2
jacob[1,5]=-(-I2*S1*beta21-I1*S1*beta11)/(S2+S1+R2+R1+I2+I1)^2-(S1*beta21)/(S2+S1+R2+R1+I2+I1)
# df(S1)/dR2
jacob[1,6]=-(-I2*S1*beta21-I1*S1*beta11)/(S2+S1+R2+R1+I2+I1)^2

# df(I1)/dS1
jacob[2,1]=(I2*beta21+I1*beta11)/(S2+S1+R2+R1+I2+I1)-(I2*S1*beta21+I1*S1*beta11)/(S2+S1+R2+R1+I2+I1)^2
# df(I1)/dI1
jacob[2,2]=-upsilon1-(I2*S1*beta21+I1*S1*beta11)/(S2+S1+R2+R1+I2+I1)^2+(S1*beta11)/(S2+S1+R2+R1+I2+I1)-alpha1-b1
# df(I1)/dR1
jacob[2,3]=-(I2*S1*beta21+I1*S1*beta11)/(S2+S1+R2+R1+I2+I1)^2
# df(I1)/dS2
jacob[2,4]=-(I2*S1*beta21+I1*S1*beta11)/(S2+S1+R2+R1+I2+I1)^2
# df(I1)/dI2
jacob[2,5]=(S1*beta21)/(S2+S1+R2+R1+I2+I1)-(I2*S1*beta21+I1*S1*beta11)/(S2+S1+R2+R1+I2+I1)^2
# df(I1)/dR2
jacob[2,6]=-(I2*S1*beta21+I1*S1*beta11)/(S2+S1+R2+R1+I2+I1)^2

# df(R1)/dS1
jacob[3,1]=0
# df(R1)/dI1
jacob[3,2]=upsilon1
# df(R1)/dR1
jacob[3,3]=-gamma1-b1
# df(R1)/dS2
jacob[3,4]=0
# df(R1)/dI2
jacob[3,5]=0
# df(R1)/dR2
jacob[3,6]=0

# df(S2)/dS1
jacob[4,1]=-(-I2*S2*beta22-I1*S2*beta12)/(S2+S1+R2+R1+I2+I1)^2
# df(S2)/dI1
jacob[4,2]=-(-I2*S2*beta22-I1*S2*beta12)/(S2+S1+R2+R1+I2+I1)^2-(S2*beta12)/(S2+S1+R2+R1+I2+I1)
# df(S2)/dR1
jacob[4,3]=-(-I2*S2*beta22-I1*S2*beta12)/(S2+S1+R2+R1+I2+I1)^2
# df(S2)/dS2
jacob[4,4]=-(-I2*S2*beta22-I1*S2*beta12)/(S2+S1+R2+R1+I2+I1)^2+(-I2*beta22-I1*beta12)/(S2+S1+R2+R1+I2+I1)+(S2*a2+R2*a2+I2*a2)/K2+((S2+R2+I2)/K2+1)*a2
# df(S2)/dI2
jacob[4,5]=-(-I2*S2*beta22-I1*S2*beta12)/(S2+S1+R2+R1+I2+I1)^2-(S2*beta22)/(S2+S1+R2+R1+I2+I1)+(S2*a2+R2*a2+I2*a2)/K2+((S2+R2+I2)/K2+1)*a2
# df(S2)/dR2
jacob[4,6]=gamma2-(-I2*S2*beta22-I1*S2*beta12)/(S2+S1+R2+R1+I2+I1)^2+(S2*a2+R2*a2+I2*a2)/K2+((S2+R2+I2)/K2+1)*a2

# df(I2)/dS1
jacob[5,1]=-(I2*S2*beta22+I1*S2*beta21)/(S2+S1+R2+R1+I2+I1)^2
# df(I2)/dI1
jacob[5,2]=(S2*beta21)/(S2+S1+R2+R1+I2+I1)-(I2*S2*beta22+I1*S2*beta21)/(S2+S1+R2+R1+I2+I1)^2
# df(I2)/dR1
jacob[5,3]=-(I2*S2*beta22+I1*S2*beta21)/(S2+S1+R2+R1+I2+I1)^2
# df(I2)/dS2
jacob[5,4]=(I2*beta22+I1*beta21)/(S2+S1+R2+R1+I2+I1)-(I2*S2*beta22+I1*S2*beta21)/(S2+S1+R2+R1+I2+I1)^2
# df(I2)/dI2
jacob[5,5]=-upsilon2-(I2*S2*beta22+I1*S2*beta21)/(S2+S1+R2+R1+I2+I1)^2+(S2*beta22)/(S2+S1+R2+R1+I2+I1)-alpha2-b2
# df(I2)/dR2
jacob[5,6]=-(I2*S2*beta22+I1*S2*beta21)/(S2+S1+R2+R1+I2+I1)^2

# df(R2)/dS1
jacob[6,1]=0
# df(R2)/dI1
jacob[6,2]=0
# df(R2)/dR1
jacob[6,3]=0
# df(R2)/dS2
jacob[6,4]=0
# df(R2)/dI2
jacob[6,5]=upsilon2
# df(R2)/dR2
jacob[6,6]=-gamma2-b2

eigen(jacob)
sum(diag(jacob))

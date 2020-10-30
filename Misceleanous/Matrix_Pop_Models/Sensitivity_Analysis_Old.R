# Sensitivity analysis
#

# parameter space
brange = seq(0.01,0.6,length.out = 100)
srange = seq(0.01,0.6,length.out = 100)

# result array
results=matrix(nrow=length(brange),ncol=length(srange))

# simulations
for (i in 1:length(brange)){
  b=brange[i]
  for (j in 1:length(srange)){
    s=srange[j]
    mymatrix=matrix(0,nrow=4,ncol=4)
    mymatrix[1,4]=b
    diag(mymatrix[-1,-ncol(mymatrix)])=s # sub-diagonal = s
    eig=eigen(mymatrix)
    results[i,j]=max(abs(eig$values))
  }
}

contour(brange,srange,results,xlab='b',ylab='s',cex.lab=2)

# sensitivity
library(matlib)
library(QZ)

b=0.3
s=0.6
mymatrix=matrix(0,nrow=4,ncol=4)
mymatrix[1,4]=b
diag(mymatrix[-1,-ncol(mymatrix)])=s

eig=eigen(mymatrix)
d=eig$values
W=eig$vectors

#imax=which(d==max(d))
ad=abs(d)
imax=which(ad==max(ad))[1]
V=Conj(solve(W))
w=W[,imax]
v=t(Re(V[imax,]))
senmat=t(v)%*%w

# sensitivity
senmat=mymatrix
for (i in 1:length(d)){
  V=Conj(solve(W))
  w=W[,i]
  v=t(Re(V[i,]))
  senmat[i,]=v*H(w)
}

contour(Re(senmat))

# elasticity
emat=senmat*mymatrix/(max(ad))
emat=Re(emat)

contour(abs(emat))

persp(seq(1,4),seq(1,4),Re(senmat),theta=150,phi=30)
persp(seq(1,4),seq(1,4),Re(emat),theta=150,phi=30)

library(barplot3d)

index=1
test=rep(0,16)
for (i in 1:4){
  for (j in 1:4){
    test[index]=emat[i,j]
    index=index+1
  }
}
legoplot3d(test)

heatmap(abs(emat))


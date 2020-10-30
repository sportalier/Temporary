# Sensitivity and elasticity

#### Parameters ####
b=0.02
s=0.8

#### Transition matrix ####
matrixdim = 4 # to change if not a 5*5 matrix

A=matrix(0,nrow = matrixdim,ncol = matrixdim)
A[1,matrixdim] = b 
diag(A[-1,-ncol(A)])=s
A[matrixdim,matrixdim]=s

#### Eigen decomposition ####
eig=eigen(A)
d=eig$values
W=eig$vectors

#### Sensitivity ####
ad=abs(d)
imax=which(ad==max(ad))[1]
V=Conj(solve(W))
w=W[,imax]
v=t(Re(V[imax,]))
senmat=t(v)%*%w
senmat=Re(senmat) # sensitivity matrix
senmat=round(senmat,digits=6)

#### Elasticity ####
emat=senmat*A/(max(ad)) # elasticity matrix

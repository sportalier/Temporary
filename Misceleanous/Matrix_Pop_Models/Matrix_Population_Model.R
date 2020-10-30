# Matrix population model with between year variability
# 11 March 2020

#### parameters ####
b1 = 0.2
s1 = 0.8

b2 = 0.1
s2 = 0.6

duration = 100
dimatrix = 4

numsimul = 1000

#### initial conditions ####
g1 = 3000
g2 = 3000
g3 = 3000
g4 = 3000

x0 = matrix(nrow = dimatrix, ncol = 1)
x0[1] = g1
x0[2] = g2
x0[3] = g3
x0[4] = g4

#### transition matrices ####
A1 = matrix(0,nrow = dimatrix, ncol = dimatrix)
diag(A1[-1,-dimatrix]) = s1
A1[dimatrix,dimatrix] = s1
A1[1,dimatrix] = b1

A2 = matrix(0,nrow = dimatrix, ncol = dimatrix)
diag(A2[-1,-dimatrix]) = s2
A2[dimatrix,dimatrix] = s2
A2[1,dimatrix] = b2

#### model runs ####
record = rep(0,numsimul)

for (j in 1:numsimul){
  event = runif(duration)
  x = x0
  for (i in 1:duration){
    if (event[i]<0.5){
      y = A2%*%x
    }else{
      y = A1%*%x
    }
    x = y
  }
  record[j]=sum(x)
}




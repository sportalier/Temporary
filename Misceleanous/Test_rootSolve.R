# test rootSolve
library(rootSolve)

model = function(t,x,parms){
  with (as.list(c(y, pars)),{
    oxicmin   = r*OM*(O2/(O2+ks))
    anoxicmin = r*OM*(1-O2/(O2+ks))* SO4/(SO4+ks2)
    dOM  = Flux - oxicmin - anoxicmin
    dO2  = -oxicmin       -2*rox*HS*(O2/(O2+ks)) + D*(BO2-O2)
    dSO4 = -0.5*anoxicmin   +rox*HS*(O2/(O2+ks)) + D*(BSO4-SO4)
    dHS  =  0.5*anoxicmin   -rox*HS*(O2/(O2+ks)) + D*(BHS-HS)
    list(c(dOM, dO2, dSO4, dHS), SumS = SO4+HS)
  })
}

pars <- c(D = 1, Flux = 100, r = 0.1, rox = 1,
          ks = 1, ks2 = 1, BO2 = 100, BSO4 = 10000, BHS = 0)
y <- c(OM = 1, O2 = 1, SO4 = 1, HS = 1)
print(system.time(RS <- runsteady(y = y, fun = model,parms = pars, times = c(0, 1e5))))




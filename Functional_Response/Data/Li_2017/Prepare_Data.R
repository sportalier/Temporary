# Prepare data
# by Sebastien Portalier
# 28 June 2020

setwd("C:/Users/Sebastien/Documents/Temporary-master/Functional_Response/Data/Li_2017")

dat=read.csv('Lietal_oikos_2017_data.csv')
dat=subset(dat,dat$predation.strategy=='predator')
dat=droplevels(dat)
dat$PredatorMass = dat$predator.mass.mg / 1e6
dat$PreyMass = dat$prey.mass.mg / 1e6

terre=subset(dat,dat$ecosystem.type=='terrestrial')
terre=droplevels(terre)
water=subset(dat,dat$ecosystem.type!='terrestrial')
water=droplevels(water)

dataterre = terre[,c(3,8,22,14,23,16,17)]
datawater = water[,c(3,8,22,14,23,16,17)]

write.csv(dataterre,'Data_Terrestrial.csv',quote = F,row.names = F)
write.csv(datawater,'Data_Aquatic.csv',quote = F,row.names = F)

matlabterre = dataterre[,c(3,5)]
matlabwater = datawater[,c(3,5)]

write.table(matlabterre,'MatlabTerre.txt',quote = F,row.names = F,col.names = F)
write.table(matlabwater,'MatlabWater.txt',quote = F,row.names = F,col.names = F)


#### Analysis ####
setwd("~/Temporary-master/Functional_Response/Data/Li_2017")

#### water
aquatic = read.csv('Data_Aquatic.csv')
handlingwater = read.table('Handling_Time_Aquatic_Matlab_Li.txt')
predatorwater = read.table('Predator_Traits_Aquatic_Matlab_Li.txt',h=T)
preywater = read.table('Prey_Traits_Aquatic_Matlab_Li.txt',h=T)
probaquatic = read.table('Capture_Prob_Aquatic_Matlab_Li.txt')

# beta
speedpred = predatorwater$ForSpeed
speedprey = preywater$ForSpeed
detectpred = predatorwater$Detection

beta = pi*detectpred^2*(speedprey^2 +3*speedpred^2)/(speedpred^2)

# attack rate
attack = beta*probaquatic

aquatic$PredictedAttack = attack
aquatic$PredictedHandling = handlingwater
names(aquatic)[8] = 'PredictedAttack'
names(aquatic)[9] = 'PredictedHandling'

plot(log10(aquatic$attack.rate)~log10(aquatic$PredatorMass),ylim=c(-12,0))
points(log10(attack$V1)~log10(aquatic$PredatorMass),pch=20,col='red')

plot(log10(aquatic$handling.time)~log10(aquatic$PredatorMass))
points(log10(handlingwater$V1)~log10(aquatic$PredatorMass),pch=20,col='red')


##### terrestrial
terre = read.csv('Data_Terrestrial.csv')
handlingterre = read.table('Handling_Time_Terre_Matlab_Li.txt')

predatorterre = read.table('Predator_Traits_Terre_Matlab_Li.txt',h=T)
preyterre = read.table('Prey_Traits_Terre_Matlab_Li.txt',h=T)
probterre = read.table('Capture_Prob_Terre_Matlab_Li.txt')

# beta
speedpred = predatorterre$ForSpeed
speedprey = preyterre$ForSpeed
detectpred = predatorterre$Detection

beta = pi*detectpred^2*(speedprey^2 +3*speedpred^2)/(speedpred^2)

# attack rate
attack = beta*probterre

aquatic$PredictedAttack = attack
aquatic$PredictedHandling = handlingwater
names(aquatic)[8] = 'PredictedAttack'
names(aquatic)[9] = 'PredictedHandling'

plot(log10(terre$attack.rate)~log10(terre$PredatorMass),ylim=c(-12,0))
points(log10(attack$V1)~log10(terre$PredatorMass),pch=20,col='red')

plot(log10(terre$handling.time)~log10(terre$PredatorMass))
points(log10(handlingterre$V1)~log10(terre$PredatorMass),pch=20,col='red')

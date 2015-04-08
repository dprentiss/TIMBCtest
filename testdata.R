setwd("C:/Users/Civil User/Box Sync/TIM-BC/R")
source('./reference_interp.R')

fc_cars <- subset(simMelt, simMelt$depvars == "fc_car")

lanes <- vector()
duration <- vector()
spd <- vector()
vol <- vector()

xl <- vector()
yl <- vector()
zl <- vector()
xu <- vector()
yu <- vector()
zu <- vector()

Wlll <- vector()
Wllu <- vector()
Wlul <- vector()
Wluu <- vector()
Wull <- vector()
Wulu <- vector()
Wuul <- vector()
Wuuu <- vector()

FClll <- vector()
FCllu <- vector()
FClul <- vector()
FCluu <- vector()
FCull <- vector()
FCulu <- vector()
FCuul <- vector()
FCuuu <- vector()

fc <- vector()

uniqueDurations <- unique(fc_cars$duration)
uniqueSpds <- unique(fc_cars$spd)
uniqueVols <- unique(fc_cars$vol)

for (i in 1:100) {
  lanes[i] <- as.character(sample(unique(fc_cars$variable), 1))
  duration[i] <- runif(1, 1, 14400)
  spd[i] <- runif(1, 60, 120)
  vol[i] <- runif(1, 500, 2200)
  result <- interpolateValue(lanes[i], duration[i], spd[i], vol[i])
  # make some 2D cases
  if (i > 50 & i < 75) {
    if (i %% 3 == 0) duration[i] <- sample(uniqueDurations, 1)
    if (i %% 3 == 1) spd[i] <- sample(uniqueSpds, 1)
    if (i %% 3 == 2) vol[i] <- sample(uniqueVols, 1)
  }
  # make some 1D cases
  if (i >= 75 & i < 90) {
    if (i %% 3 == 0) {
      duration[i] <- sample(uniqueDurations, 1)
      spd[i] <- sample(uniqueSpds, 1)
    }  
    if (i %% 3 == 1) {
      spd[i] <- sample(uniqueSpds, 1)
      vol[i] <- sample(uniqueVols, 1)
    }
    if (i %% 3 == 2) {
      vol[i] <- sample(uniqueVols, 1)
      duration[i] <- sample(uniqueDurations, 1)
    }
  }
  # make some point cases
  if (i >= 90) {
    duration[i] <- sample(uniqueDurations, 1)
    spd[i] <- sample(uniqueSpds, 1)
    vol[i] <- sample(uniqueVols, 1)
  }
  result <- interpolateValue(lanes[i], duration[i], spd[i], vol[i])
  
  
  xl[i] <- result[1] 
  yl[i] <- result[2]
  zl[i] <- result[3]
  xu[i] <- result[4]
  yu[i] <- result[5]
  zu[i] <- result[6]
  
  Wlll[i] <- result[7]
  Wllu[i] <- result[8]
  Wlul[i] <- result[9]
  Wluu[i] <- result[10]
  Wull[i] <- result[11]
  Wulu[i] <- result[12]
  Wuul[i] <- result[13]
  Wuuu[i] <- result[14]
  
  FClll[i] <- result[15]
  FCllu[i] <- result[16]
  FClul[i] <- result[17]
  FCluu[i] <- result[18]
  FCull[i] <- result[19]
  FCulu[i] <- result[20]
  FCuul[i] <- result[21]
  FCuuu[i] <- result[22]
  
  fc[i] <- result[23]
  
  interpTestData <- data.frame(lanes, duration, spd, vol, xl, yl, zl, xu, yu, zu, Wlll, Wllu, Wlul, Wluu, Wull, Wulu, Wuul, Wuuu, FClll, FCllu, FClul, FCluu, FCull, FCulu, FCuul, FCuuu, fc)
}


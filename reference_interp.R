fcTableLookup <- function(lanes, duration, spd, vol) {
  # returns the fuel consuption value for the inputs from the table
  # Inputs must match table values exactly. This is not enforced in the reference implementation.
  return(fc_cars[fc_cars$vol == vol & fc_cars$spd == spd & fc_cars$duration == duration & fc_cars$variable == lanes,]$value)
}

interpolateValue <- function(lanes, duration, spd, vol) {
  
  # use concise variable names for conveniece
  xc <- duration  # duration input
  yc <- spd       # FFS input
  zc <- vol       # volume input
  
  # get unique values from table
  # This only needs to be done once but is included here for clarity
  # Comment out if unique values are supplied somewhere else
  uniqueDurations <- unique(fc_cars$duration)
  # 1   300   600   900  1200  1500  1800  2100  2400  2700  3000  3300  3600  3900  4200  4500  4800  5100  5400  5700  6000  6300  6600  6900  7200  9000 10800 12600 14400
  uniqueSpds <- unique(fc_cars$spd)
  # 60  70  80  90 100 120
  uniqueVols <- unique(fc_cars$vol)
  # 500 1000 1200 1400 1600 1800 2000 2200
  
  
  # get the next highest and next lowest input values from the table
  # For each input, the next highest and next lowest unique input values in the table are found.
  xl <- max(uniqueDurations[uniqueDurations <= duration])
  yl <- max(uniqueSpds[uniqueSpds <= spd])
  zl <- max(uniqueVols[uniqueVols <= vol])
  xu <- min(uniqueDurations[uniqueDurations >= duration])
  yu <- min(uniqueSpds[uniqueSpds >= spd])
  zu <- min(uniqueVols[uniqueVols >= vol])
  
  # check if some inputs are exact matches to table, normalize if so
  # If an input happens to be exactly equal to a table input value, we need to normalize that input to avoid division by zero.
  if (xl == xu) xl = xu - 1
  if (yl == yu) yl = yu - 1
  if (zl == zu) zl = zu - 1
  
  # calculate weights
  # These are the diagonally opposing volume proportions
  # The denominator is always the total volume: (xu - xl) / (yu - yl) / (zu - zl)
  Wlll <- (xu - xc) * (yu - yc) * (zu - zc) / (xu - xl) / (yu - yl) / (zu - zl)
  Wllu <- (xu - xc) * (yu - yc) * (zc - zl) / (xu - xl) / (yu - yl) / (zu - zl)
  Wlul <- (xu - xc) * (yc - yl) * (zu - zc) / (xu - xl) / (yu - yl) / (zu - zl)
  Wluu <- (xu - xc) * (yc - yl) * (zc - zl) / (xu - xl) / (yu - yl) / (zu - zl)
  Wull <- (xc - xl) * (yu - yc) * (zu - zc) / (xu - xl) / (yu - yl) / (zu - zl)
  Wulu <- (xc - xl) * (yu - yc) * (zc - zl) / (xu - xl) / (yu - yl) / (zu - zl)
  Wuul <- (xc - xl) * (yc - yl) * (zu - zc) / (xu - xl) / (yu - yl) / (zu - zl)
  Wuuu <- (xc - xl) * (yc - yl) * (zc - zl) / (xu - xl) / (yu - yl) / (zu - zl)

  # reset lower nearest points for table lookup
  # If we normalized an input in a previous step we need to replace the original value so that table lookup does not fail.
  xl <- max(uniqueDurations[uniqueDurations <= duration])
  yl <- max(uniqueSpds[uniqueSpds <= spd])
  zl <- max(uniqueVols[uniqueVols <= vol])

  # lookup fuel consumption at nearest points from table
  FClll <- fcTableLookup(lanes, xl, yl, zl)
  FCllu <- fcTableLookup(lanes, xl, yl, zu)
  FClul <- fcTableLookup(lanes, xl, yu, zl)
  FCluu <- fcTableLookup(lanes, xl, yu, zu)
  FCull <- fcTableLookup(lanes, xu, yl, zl)
  FCulu <- fcTableLookup(lanes, xu, yl, zu)
  FCuul <- fcTableLookup(lanes, xu, yu, zl)
  FCuuu <- fcTableLookup(lanes, xu, yu, zu)
  
  # calculate fuel consumption from table points and weights
  FCvalue <- FClll * Wlll +
           FCllu * Wllu +
           FClul * Wlul +
           FCluu * Wluu +
           FCull * Wull +
           FCulu * Wulu +
           FCuul * Wuul +
           FCuuu * Wuuu  
  
  # returns intepolated value and test data
  # comment out for production
  # return(c(xl, yl, zl, xu, yu, zu, Wlll, Wllu, Wlul, Wluu, Wull, Wulu, Wuul, Wuuu, FClll, FCllu, FClul, FCluu, FCull, FCulu, FCuul, FCuuu, FCvalue))
  
  # returns interpolated value only
  # comment out for testing
  return(FCvalue)
}

interpolateValue2 <- function(numLanes, lanesBlocked, duration, spd, vol) {
  
  lanes <- switch(numLanes,
                      ,
                      switch(lanesBlocked + 1,
                        "two_zero",
                        "two_one"
                        ),
                      switch(lanesBlocked + 1,
                        "three_zero",
                        "three_one",
                        "three_two"
                        ),
                      switch(lanesBlocked + 1,
                        "four_zero",
                        "four_one",
                        "four_two",
                        "four_three"
                        ),
                      switch(lanesBlocked + 1,
                        "five_zero",
                        "five_one",
                        "five_two",
                        "five_three",
                        "five_four"
                        ),
                      switch(lanesBlocked + 1,
                        "six_zero",
                        "six_one",
                        "six_two",
                        "six_three",
                        "six_four"
                        )
                      ) 
  
  # use concise variable names for conveniece
  duration <- duration * 60
  xc <- duration  # duration input
  yc <- spd       # FFS input
  zc <- vol       # volume input
  
  # get unique values from table
  # This only needs to be done once but is included here for clarity
  # Comment out if unique values are supplied somewhere else
  uniqueDurations <- unique(fc_cars$duration)
  # 1   300   600   900  1200  1500  1800  2100  2400  2700  3000  3300  3600  3900  4200  4500  4800  5100  5400  5700  6000  6300  6600  6900  7200  9000 10800 12600 14400
  uniqueSpds <- unique(fc_cars$spd)
  # 60  70  80  90 100 120
  uniqueVols <- unique(fc_cars$vol)
  # 500 1000 1200 1400 1600 1800 2000 2200
  
  
  # get the next highest and next lowest input values from the table
  # For each input, the next highest and next lowest unique input values in the table are found.
  xl <- max(uniqueDurations[uniqueDurations <= duration])
  yl <- max(uniqueSpds[uniqueSpds <= spd])
  zl <- max(uniqueVols[uniqueVols <= vol])
  xu <- min(uniqueDurations[uniqueDurations >= duration])
  yu <- min(uniqueSpds[uniqueSpds >= spd])
  zu <- min(uniqueVols[uniqueVols >= vol])
  
  # check if some inputs are exact matches to table, normalize if so
  # If an input happens to be exactly equal to a table input value, we need to normalize that input to avoid division by zero.
  if (xl == xu) xl = xu - 1
  if (yl == yu) yl = yu - 1
  if (zl == zu) zl = zu - 1
  
  # calculate weights
  # These are the diagonally opposing volume proportions
  # The denominator is always the total volume: (xu - xl) / (yu - yl) / (zu - zl)
  Wlll <- (xu - xc) * (yu - yc) * (zu - zc) / (xu - xl) / (yu - yl) / (zu - zl)
  Wllu <- (xu - xc) * (yu - yc) * (zc - zl) / (xu - xl) / (yu - yl) / (zu - zl)
  Wlul <- (xu - xc) * (yc - yl) * (zu - zc) / (xu - xl) / (yu - yl) / (zu - zl)
  Wluu <- (xu - xc) * (yc - yl) * (zc - zl) / (xu - xl) / (yu - yl) / (zu - zl)
  Wull <- (xc - xl) * (yu - yc) * (zu - zc) / (xu - xl) / (yu - yl) / (zu - zl)
  Wulu <- (xc - xl) * (yu - yc) * (zc - zl) / (xu - xl) / (yu - yl) / (zu - zl)
  Wuul <- (xc - xl) * (yc - yl) * (zu - zc) / (xu - xl) / (yu - yl) / (zu - zl)
  Wuuu <- (xc - xl) * (yc - yl) * (zc - zl) / (xu - xl) / (yu - yl) / (zu - zl)
  
  # reset lower nearest points for table lookup
  # If we normalized an input in a previous step we need to replace the original value so that table lookup does not fail.
  xl <- max(uniqueDurations[uniqueDurations <= duration])
  yl <- max(uniqueSpds[uniqueSpds <= spd])
  zl <- max(uniqueVols[uniqueVols <= vol])
  
  # lookup fuel consumption at nearest points from table
  FClll <- fcTableLookup(lanes, xl, yl, zl)
  FCllu <- fcTableLookup(lanes, xl, yl, zu)
  FClul <- fcTableLookup(lanes, xl, yu, zl)
  FCluu <- fcTableLookup(lanes, xl, yu, zu)
  FCull <- fcTableLookup(lanes, xu, yl, zl)
  FCulu <- fcTableLookup(lanes, xu, yl, zu)
  FCuul <- fcTableLookup(lanes, xu, yu, zl)
  FCuuu <- fcTableLookup(lanes, xu, yu, zu)
  
  # calculate fuel consumption from table points and weights
  FCvalue <- FClll * Wlll +
    FCllu * Wllu +
    FClul * Wlul +
    FCluu * Wluu +
    FCull * Wull +
    FCulu * Wulu +
    FCuul * Wuul +
    FCuuu * Wuuu  
  
  # returns intepolated value and test data
  # comment out for production
  # return(c(xl, yl, zl, xu, yu, zu, Wlll, Wllu, Wlul, Wluu, Wull, Wulu, Wuul, Wuuu, FClll, FCllu, FClul, FCluu, FCull, FCulu, FCuul, FCuuu, FCvalue))
  
  # returns interpolated value only
  # comment out for testing
  return(FCvalue)
}

# FCc <- function(numLanes, lanesBlocked, durationMins, speedLimit, numRamps, segmentLength,
#                 percentTrucks, trafficVolume, gradient) {
#   lanes <- switch(numLanes,
#     ,
#     switch(lanesBlocked + 1,
#       "two_zero",
#       "two_one"
#       ),
#     switch(lanesBlocked + 1,
#       "three_zero",
#       "three_one",
#       "three_two"
#       ),
#     switch(lanesBlocked + 1,
#       "four_zero",
#       "four_one",
#       "four_two",
#       "four_three"
#       ),
#     switch(lanesBlocked + 1,
#       "five_zero",
#       "five_one",
#       "five_two",
#       "five_three",
#       "five_four"
#       ),
#     switch(lanesBlocked + 1,
#       "six_zero",
#       "six_one",
#       "six_two",
#       "six_three",
#       "six_four"
#       )
#     ) 
#   duration <- durationMins*60
#   spd <- (speedLimit-(numRamps/segmentLength*5/2))*1.609344
#   vol <- trafficVolume
# 
#   fuelConsumption <- interpolateValue(lanes, duration, spd, vol)
# 
#   return(fuelConsumption + 0.001*percentTrucks - 0.015*(gradient) + 0.001*(gradient^2))
# }

fuelgtogal <- function(g, density=0.00036301992) {
  return(g*density)
}

fuelToClipboard <- function() {
  clip <- read.table("clipboard", sep="\t")
  withSSP <- mapply(interpolateValue2, clip$V5, clip$V6, clip$V14, clip$V24, clip$V11)
  withoutSSP <- mapply(interpolateValue2, clip$V5, clip$V6, clip$V14+clip$V1, clip$V24, clip$V11)
  write.table(cbind(withSSP,withoutSSP), "clipboard", sep="\t", row.names=FALSE, col.names=FALSE)
}

randSeg <- function() {
  
}
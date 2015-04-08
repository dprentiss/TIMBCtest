#setwd("C:/Users/Civil User/Box Sync/TIM-BC/tables")
setwd("C:/Users/Dave/Box Sync/TIM-BC/tables")

library(reshape2)
library(ggplot2)
library(grid)
library(gridExtra)

fileNames <- c("fuel_consumption_car.csv",
               "travel_delay_car.csv",
               "travel_delay_truck.csv",
               "average_speed_car.csv")

varNames <- c("Total fuel consumption",
              "Travel delay, cars",
              "Travel delay, trucks",
              "Average speed, cars")

outFileName <- "sim_data_graphs.pdf"

simData <- list()
simDataAll <- data.frame()
plots <- list()

simLabeller <- function(var, value){
  value <- as.character(value)
  if (var=="depvars") { 
    value[value=="fc_car"] <- "Fuel consumption, cars"
    value[value=="td_car"] <- "Travel delay, cars"
    value[value=="td_hgv"] <- "Travel delay, trucks"
    value[value=="as_cars"] <- "Average speed, cars"
  }
  return(value)
}

# load CSV files
for (depVar in 1:4) {  # load CSV files
  simData[[depVar]] <- read.csv(fileNames[depVar])
}

# init column names
simDataAll <- merge(simDataAll, simData[[1]], all = T)

# merge data to single dataframe
for (depVar in 1:4) {  
  simDataAll <- merge(simDataAll, simData[[depVar]], all = T)  
}

# melt data for plotting
simMelt <- melt(simDataAll, id.vars=c("duration", "spd", "vol", "depvars"), na.rm=F)

simSplit <- split(simMelt, simMelt$variable)


pdf(outFileName, paper = "letter", height = 10, width = 7.5)
for (laneCombo in 1:19) {
plots[[laneCombo]] <- ggplot(simSplit[[laneCombo]], aes(x=duration, y=value, group = interaction(vol,spd), col = as.factor(vol), shape = as.factor(spd))) +
  geom_point() + geom_line() +
  labs(shape = "FFS", colour = "Volume", title = sprintf("lanes_blocked = %s", names(simDataAll[laneCombo+3])), y = "", x = "Duration") +
  facet_grid(depvars ~ ., scales = "free_y", labeller=simLabeller)
print(plots[[laneCombo]])
}
dev.off()

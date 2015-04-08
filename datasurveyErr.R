library(ggplot2)
library(grid)
library(gridExtra)

plots <- list()

for (laneCombination in 3:3) {
  for (depVar in 1:4) {
    simdata <- data.frame(cond = rep(c("A", "B"), each=10),
                                    xvar = 1:20,
                                    yvar = 1:20 + rnorm(20,sd=3))
    plots[[depVar]] <- ggplot(simdata, aes(xvar, simdata[,laneCombination])) +
    geom_point() + geom_line() + labs(x="Duration", y=depVar) + theme(legend.position="none")
  }
  grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], ncol = 1, main = colnames(simdata[laneCombination]))
}


rm(list=ls())
gc()

library(colorednoise)
library(ggplot2)
library(reshape2)

set.seed(3409)

instrumental1 <- colored_noise(timesteps = 1000, mean = 1, sd = 1, phi = 0.6)
proxy1 <- instrumental1 + rnorm(1000, mean = 0, sd=2)


plotDat1 <- data.frame("Year" = 1001:2000,
           "Instrumental" = instrumental1,
           "Proxy" = proxy1)
plotDat1$Instrumental <- plotDat1$Instrumental - mean(plotDat1$Instrumental)
plotDat1$Instrumental <- plotDat1$Instrumental/sd(plotDat1$Instrumental)

plotDat1$Proxy <- plotDat1$Proxy - mean(plotDat1$Proxy)
plotDat1$Proxy <- plotDat1$Proxy/sd(plotDat1$Proxy)

plotDat <- melt(data = plotDat1, id.vars = "Year")

ggplot(data = plotDat, mapping = aes(x=Year, y=value, group=variable, color=variable)) + geom_line() +
  xlim(c(1500,2000))

cor(plotDat1$Instrumental, plotDat1$Proxy)

mean(plotDat1$Instrumental)
sd(plotDat1$Instrumental)
mean(plotDat1$Proxy)
sd(plotDat1$Proxy)

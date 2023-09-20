#Proxy created from GISS values + ARIMA-structured noise
rm(list = ls())
gc()

noiseLevel <- runif(1,0.1,1)

setwd("C:/Users/dce72/Downloads")
gisTemp <- read.csv("GISTEMPv4.csv")


noiseShape <- list("p" = round(runif(1,0,3)), 
                   "d" = round(runif(1,0,1)), 
                   "q" = round(runif(1,0,3)))


noise1 <- arima.sim(noiseShape, length(gisTemp$GISTEMPv4)) * (noiseLevel)
proxy0 <- (gisTemp$GISTEMPv4 + noise1)

proxy1 <- proxy0 * sd((gisTemp$GISTEMPv4))/sd((proxy0))

df1 <- data.frame("Year" = gisTemp$Year,
                  "GISS" = gisTemp$GISTEMPv4,
                  "proxy" = proxy1)


require(reshape2)
require(ggplot2)

plotData <- melt(df1, id.vars = "Year")

ggplot(mapping = aes(x=Year, y=value, color=variable), data=plotData) + geom_line()

summary(lm(GISS~proxy , data = df1))

SPlen <- 5
df1$proxy2 <-  dplR::detrend.series((df1$proxy + 1), method = "Spline", nyrs = SPlen, return.info = TRUE)$curves - 1
plotData <- melt(df1[,-3], id.vars = "Year")
ggplot(mapping = aes(x=Year, y=value, color=variable), data=plotData) + geom_line()
cor(df1)

sd(diff(df1$GISS))
sd(diff(df1$proxy))
sd(diff(df1$proxy2))

SDdiff <- .05
while (abs(SDdiff) > .01){
  SDdiff1 <- (1 - sd(diff(df1$proxy2))/sd(diff(df1$GISS)))
  if (SDdiff1 > 0){
    SPlen <- SPlen - SPlen * 0.05
  } else if (SDdiff1 < 0){
    SPlen <- SPlen + SPlen * 0.05}
  df1$proxy2 <-  dplR::detrend.series((df1$proxy + 1), method = "Spline", nyrs = SPlen, return.info = TRUE)$curves - 1
  SDdiff <- abs(1 - sd(diff(df1$proxy2))/sd(diff(df1$GISS)))
}

sd(diff(df1$GISS))
sd(diff(df1$proxy))
sd(diff(df1$proxy2))

df1$proxy2 <- df1$proxy2 * sd(df1$GISS)/sd(df1$proxy2)

plotData <- melt(df1[,-3], id.vars = "Year")
ggplot(mapping = aes(x=Year, y=value, color=variable), data=plotData) + geom_line()
cor(df1)

instrumentalARIMA <- forecast::auto.arima(gisTemp$GISTEMPv4)
proxyARIMA <- forecast::auto.arima(df1$proxy2)
noiseARIMA <- forecast::auto.arima(noise1)

#Print out the noise input level, ARIMA structures of the original and proxy, 
#correlation, normality test of the residuals, and autocorrelation of the residuals
#noise input level
round(noiseLevel/1*100)
#noise input structure
noiseARIMA$coef
#Instrumental ARIMA
instrumentalARIMA$coef
#Proxy ARIMA
proxyARIMA$coef
#Instrumental-proxy correlation
cor.test(df1$GISS, df1$proxy2)
#Look at residuals
shapiro.test(df1$GISS - df1$proxy2)
hist(df1$GISS - df1$proxy2)
plot(df1$Year, df1$GISS - df1$proxy2)
#Autocorrelation of residuals
pacf(df1$GISS - df1$proxy2)

#################################################################
#################################################################
#Proxy values scaled in relation to the standard deviation of the first difference of GISS values
#Noise added to first differences

rm(list = ls())
gc()

noiseLevel <- 6

setwd("C:/Users/dce72/Downloads")
gisTemp <- read.csv("GISTEMPv4.csv")

noiseShape <- list("p" = 0, "d" = 0, "q" = 0)

GISSdiff <- diff(gisTemp$GISTEMPv4)


noise1 <- arima.sim(noiseShape, length(GISSdiff)) * (0.01 * noiseLevel)
proxy0 <- (GISSdiff + noise1)

proxy1 <- proxy0 * sd((GISSdiff))/sd((proxy0))

df1 <- data.frame("Year" = gisTemp$Year[-1],
                  "GISS" = GISSdiff,
                  "proxy" = proxy1)


require(reshape2)
require(ggplot2)

plotData <- melt(df1, id.vars = "Year")

ggplot(mapping = aes(x=Year, y=value, color=variable), data=plotData) + geom_line()

summary(lm(GISS~proxy , data = df1))

df2 <- data.frame("Year" = gisTemp$Year,
                  "GISS" = cumsum(c(gisTemp$GISTEMPv4[1], GISSdiff)),
                  "proxy" = cumsum(c(gisTemp$GISTEMPv4[1], proxy1)) + (mean(cumsum(GISSdiff))-mean(cumsum(proxy1))))

plotData2 <- melt(df2, id.vars = "Year")

ggplot(mapping = aes(x=Year, y=value, color=variable), data=plotData2) + geom_line()

summary(lm(GISS~proxy , data = df2))

#################################################################
#################################################################
#Impart noice at all frequencies

rm(list = ls())
gc()

noiseLevel <- 10

setwd("C:/Users/dce72/Downloads")
gisTemp <- read.csv("GISTEMPv4.csv")

TempApprox <- data.frame("Year" = gisTemp$Year,
                         "GIStemp" = gisTemp$GISTEMPv4,
                         "S1" = rep(NA, length(gisTemp$GISTEMPv4)),
                         "S2" = rep(NA, length(gisTemp$GISTEMPv4)),
                         "S3" = rep(NA, length(gisTemp$GISTEMPv4)),
                         "S4" = rep(NA, length(gisTemp$GISTEMPv4)),
                         "S5" = rep(NA, length(gisTemp$GISTEMPv4)),
                         "S6" = rep(NA, length(gisTemp$GISTEMPv4)),
                         "S7" = rep(NA, length(gisTemp$GISTEMPv4)),
                         "S8" = rep(NA, length(gisTemp$GISTEMPv4)),
                         "S9" = rep(NA, length(gisTemp$GISTEMPv4)),
                         "S10" = rep(NA, length(gisTemp$GISTEMPv4)),
                         "S11" = rep(NA, length(gisTemp$GISTEMPv4)),
                         "S12" = rep(NA, length(gisTemp$GISTEMPv4)),
                         "S13" = rep(NA, length(gisTemp$GISTEMPv4)),
                         "S14" = rep(NA, length(gisTemp$GISTEMPv4)),
                         "S15" = rep(NA, length(gisTemp$GISTEMPv4)),
                         "S16" = rep(NA, length(gisTemp$GISTEMPv4)),
                         "S17" = rep(NA, length(gisTemp$GISTEMPv4)),
                         "S18" = rep(NA, length(gisTemp$GISTEMPv4)),
                         "S19" = rep(NA, length(gisTemp$GISTEMPv4)),
                         "S20" = rep(NA, length(gisTemp$GISTEMPv4)))

for (i in 1:19){
spline <- dplR::detrend.series(gisTemp$GISTEMPv4+1, method = "Spline", 
                               nyrs = (seq(2,7,length.out=19)^3)[i], return.info = TRUE, make.plot = FALSE)
TempApprox[,(i+2)] <- spline$curves-1
}

linear1 <- lm(GISTEMPv4~Year, data=gisTemp)
TempApprox[,(22)] <- linear1$fitted.values

cor(TempApprox$GIStemp, TempApprox[,3:22])

detrended0 <- TempApprox$GIStemp - TempApprox[,3:22]
detrended0 <- cbind(TempApprox$Year, detrended0)
names(detrended0)[1] <- "Year"

# summary(TempApprox)
# summary(detrended0)
# apply(detrended0[,-1], 2, function(x) sd(x))
# apply(TempApprox[,3:22], 2, function(x) sd(x))


require(ggplot2)
require(reshape2)
# 
# plotData <- melt(detrended0, id.vars = "Year")
# 
# ggplot(mapping = aes(x=Year, y=value, color=variable, group=variable), data = plotData) + geom_line()

# cor(detrended1$S20, detrended1[,2:19])
# apply(detrended1[,2:21], 2, function(x) mean(x))
# detrended1[,2:21] <- apply(detrended1[,2:21], 2, function(x) x * 1/sd(x))
# apply(detrended1[,2:21], 2, function(x) sd(x))

detrended1 <- detrended0

for(i in 2:21){
  noiseMultiplier <- runif(1, min = 0, max = noiseLevel)
  cat(noiseMultiplier, "\n")
  andSumNoise <- detrended0[,i] + rnorm(length(detrended0[,i]), mean(detrended0[,i]), noiseMultiplier * sd(detrended0[,i]))
  detrended1[,i] <- andSumNoise * sd(detrended0[,i])/sd(andSumNoise)
  #detrended1[,i] <- detrended1[,i] * sd(detrended1[,2])/sd(detrended1[,i])
}
#avgSTDEV <- mean(apply(TempApprox[,3:22], 2, function(x) sd(x)))
#detrended1[,2:21] <- apply(detrended1[,2:21], 2, function(x) x * avgSTDEV/sd(x))

#signalNoiseRatio <- runif(1, 0, 1)
#noiseAdded1 <- (signalNoiseRatio * detrended1[,2:21]) + ((1 - signalNoiseRatio) * TempApprox[,3:22])
noiseAdded1 <- detrended1[,2:21] + TempApprox[,3:22]
cor(noiseAdded1, gisTemp$GISTEMPv4)

apply(detrended1[,2:21], 2, function(x) sd(x))
apply(TempApprox[,3:22], 2, function(x) sd(x))
apply(noiseAdded1, 2, function(x) sd(x))
sd(gisTemp$GISTEMPv4)

plotData2 <- cbind(gisTemp[,1:2], noiseAdded1)
names(plotData2)[2] <- "GIStemp"


apply(plotData2[,3:22], 2, function(x) sd(x))
plotData2[,3:22] <- apply(plotData2[,3:22], 2, function(x) x*(sd(plotData2[,2]))/sd(x))
cor(plotData2$GIStemp, plotData2[,3:22])

plotData3 <- plotData2[,-2]
plotData3 <- melt(plotData3, id.vars = "Year")
ggplot(mapping=aes(x=Year, y=value, color=variable), data=plotData3) + geom_line() + 
  geom_line(mapping = aes(x=Year, y=GISTEMPv4), data = gisTemp, inherit.aes = FALSE, lwd=1) +
  facet_wrap(.~variable)

apply(plotData2[,2:22], 2, function(x) sd(x))
apply(plotData2[,2:22], 2, function(x) mean(x))

apply(plotData2[,2:22], 2, function(x) sd(diff(x)))

#################################################################
#################################################################


sd((df2$GISS))
sd((df2$proxy))
sd(diff(df2$GISS))
sd(diff(df2$proxy))


cor.test(df2$GISS, df2$proxy)

auto.arima(proxy0 - df1$GISS)
auto.arima(proxy1 - df1$GISS)
auto.arima(noise1)
auto.arima(proxy1)
auto.arima(gisTemp$GISTEMPv4)

summary(lm(GISS~proxy , data = df2))

df2 <- data.frame("Year" = gisTemp$Year[-1],
                  "GISSdiff" = diff(df1$GISS),
                  "Proxydiff" = diff(df1$proxy))

plotData2 <- melt(df2, id.vars = "Year")

ggplot(mapping = aes(x=Year, y=value, color=variable), data=plotData2) + geom_line()

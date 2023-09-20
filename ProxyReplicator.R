#Proxy created from GISS values + ARIMA-structured noise
rm(list = ls())
gc()

require(reshape2)
require(ggplot2)

setNumRuns <- 100

setwd("C:/Users/dce72/Downloads")
gisTemp <- read.csv("GISTEMPv4.csv") 

df1 <- data.frame("Year" = gisTemp$Year,
                  "GISS" = gisTemp$GISTEMPv4,
                  "proxy" = rep(NA, length(gisTemp$Year)))

df2 <- df1[,1:2]

for (i in 1:setNumRuns){
  noiseLevel <- runif(1,0.1,1)
  

  
  
  noiseShape <- list("p" = round(runif(1,0,3)), 
                     "d" = round(runif(1,0,1)), 
                     "q" = round(runif(1,0,3)))
  
  
  noise1 <- arima.sim(noiseShape, length(gisTemp$GISTEMPv4)) * (noiseLevel)
  proxy0 <- (gisTemp$GISTEMPv4 + noise1)
  
  proxy1 <- proxy0 * sd((gisTemp$GISTEMPv4))/sd((proxy0))
  
  df1$proxy <- proxy1
  
  # plotData <- melt(df1, id.vars = "Year")
  # 
  # ggplot(mapping = aes(x=Year, y=value, color=variable), data=plotData) + geom_line()
  # 
  # summary(lm(GISS~proxy , data = df1))
  
  SPlen <- 5
  df1$proxy2 <-  dplR::detrend.series((df1$proxy + 1), method = "Spline", nyrs = SPlen, return.info = TRUE, make.plot = FALSE)$curves - 1
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
    df1$proxy2 <-  dplR::detrend.series((df1$proxy + 1), method = "Spline", nyrs = SPlen, return.info = TRUE, make.plot = FALSE)$curves - 1
    SDdiff <- abs(1 - sd(diff(df1$proxy2))/sd(diff(df1$GISS)))
  }
  
  df1$proxy2 <- df1$proxy2 + arima.sim(noiseShape, length(gisTemp$GISTEMPv4)) * (noiseLevel * 0.1)
  
  sd(diff(df1$GISS))
  sd(diff(df1$proxy))
  sd(diff(df1$proxy2))
  
  df1$proxy2 <- df1$proxy2 * sd(df1$GISS)/sd(df1$proxy2)
  
  df2 <- cbind(df2, df1$proxy2)
}

indices1 <- c(3, setNumRuns + 2)
names(df2)[indices1[1]:indices1[2]] <- paste0("Proxy", 1:setNumRuns)

plotData <- melt(df2, id.vars = "Year")
ggplot(mapping = aes(x=Year, y=value, color=variable), data=plotData) + geom_line() +
  theme(legend.position = "none")
hist(cor(df2$GISS, df2[,indices1[1]:indices1[2]]))

instrumentalARIMA <- forecast::auto.arima(gisTemp$GISTEMPv4)
proxyARIMA <- forecast::auto.arima(df1$proxy2)
noiseARIMA <- forecast::auto.arima(noise1)
ProxyShape <- list()
for (i in indices1[1]:indices1[2]){
  ProxyShape[[i-2]] <- forecast::auto.arima(df2[,i])
}
for (i in 1:10){
  cat(names(ProxyShape[[i]]$coef), "\n")
}

names(ProxyShape[[1]]$coef)
ProxyShape[[2]]$coef
ProxyShape[[3]]$coef
names(ProxyShape[[4]]$coef)
ProxyShape[[5]]$coef
ProxyShape[[6]]$coef
ProxyShape[[7]]$coef
ProxyShape[[8]]$coef
ProxyShape[[9]]$coef
length(ProxyShape[[10]]$coef)

ARIMAstructure <- data.frame(matrix(data=NA, nrow = 100, ncol = 7))
names(ARIMAstructure) <- c("ar1", "ar2", "ar3", "ma1", "ma2", "ma3", "drift")

for (i in 1:100){
  ARIMAstructure[i,which(names(ARIMAstructure) %in% names(ProxyShape[[i]]$coef))] <- ProxyShape[[i]]$coef
}
summary(ARIMAstructure)
forecast::auto.arima(df2[,2])

structures <- list()
for (i in 1:100){
  if (names(ARIMAstructure)[1] %in% names(ProxyShape[[i]]$coef)){
    p <- max(which(names(ARIMAstructure)[1:3] %in% names(ProxyShape[[i]]$coef)))
  }else{
    p <- 0
  }
  if (names(ARIMAstructure)[4] %in% names(ProxyShape[[i]]$coef)){
    q <- max(which(names(ARIMAstructure)[4:6] %in% names(ProxyShape[[i]]$coef)))
  }else{
    q <- 0
  }
  if(names(ARIMAstructure)[7] %in% names(ProxyShape[[i]]$coef)){
    d <- which(names(ARIMAstructure)[7] %in% names(ProxyShape[[2]]$coef))
  }else{
    d <- 0
  }
  
  structures[[i]] <- c(p,d,q)
}
structures

ProxyShape[[97]]

for (i in 1:10){
  print(ProxyShape[[i]][7], "\n")
}
ProxyShape[[9]]$model$
forecast::auto.arima

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

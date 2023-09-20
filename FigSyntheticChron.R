library(dplR)
library(reshape2)
library(ggplot2)

set.seed(235)

climDat1 <- read.csv("C:/Users/dce72/Documents/GradSchool/Uncertainty/SimpleBootTrials/LangaraSST.csv")
climDat1 <- climDat1[climDat1$Year < 2002,]

TNchron <- read.rwl("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\SimpleBootTrials\\TreeNobRCStrunc1.csv")
TNchron <- TNchron[as.numeric(rownames(TNchron))>1725,]
TNchron <- TNchron[,apply(TNchron, 2, function(x) sum(!is.na(x)))>0]

TNmvchron <- chron(TNchron)

cor(TNmvchron$std[as.numeric(rownames(TNmvchron)) %in% climDat1$Year], climDat1$Target[climDat1$Year %in% as.numeric(rownames(TNmvchron))])

noiseFacor=runif(n=1, min=.1, max=3)

splen1 <- nrow(TNmvchron)/runif(1,1,10)

spline1 <- detrend.series(TNmvchron[,1], method = "Spline", return.info = TRUE, nyrs = splen1, make.plot = TRUE)

origSplit <- runif(1, .3, .7)
newsplit <- 1-origSplit

chronSur <- rep(NA, nrow(TNmvchron))
noise1 <- rep(NA, nrow(TNmvchron))

residsSD2 <- sd(spline1$series)

for (iii in 1:nrow(TNmvchron)){
  noise1[iii] <- rnorm(1, mean = spline1$series[iii], sd=residsSD2*noiseFacor)
  chronSur[iii] <- spline1$curves[iii] + (noise1[iii] * newsplit) + (spline1$series[iii] * origSplit)
}

plotdf <- data.frame(Year=as.numeric(rownames(TNchron)),
                     Original=TNmvchron[,1],
                     Low_Freq=spline1$curves,
                     High_Freq=spline1$series * origSplit,
                     Noise=noise1 * newsplit,
                     TMVSC=chronSur-1)

plotdf <- melt(plotdf, id.vars = "Year")

tail(plotdf)

ggplot(data = plotdf, mapping = aes(x=Year, y=value)) + geom_line() +
  facet_grid(variable~.)

cor(TNmvchron[,1], chronSur)

cor(chronSur[as.numeric(rownames(TNmvchron)) %in% climDat1$Year], climDat1$Target[climDat1$Year %in% as.numeric(rownames(TNmvchron))])

acf(TNmvchron[,1])$acf[2,1,1]
acf(chronSur)$acf[2,1,1]

#Individual Time Series

rbarADJ=runif(n=1, min = 0, max = 1)


TSindex <- !is.na(TNchron[,24])
TSorig <- TNchron[!is.na(TNchron[,24]),24]
TS1 <- chronSur[TSindex]
TSlen <- length(TS1)
TSsur <- rep(NA, TSlen)
noise1 <- rnorm(TSlen,mean=0,sd=sd(TS1)*rbarADJ)
spline1 <- detrend.series(TS1+noise1, method = "Spline", return.info = TRUE, nyrs = TSlen, make.plot = FALSE)
residsSD <- sd(spline1$series)

orig1 <- sqrt(rwi.stats(TNchron)$rbar.eff)
new1 <- 1-orig1

avgErr2 <- sd(TS1)#*percErr
noise2 <- rep(NA, TSlen)
highFreq2 <- rep(NA, TSlen)

for (qqq in 1:TSlen){
  noise2[qqq] <- (rnorm(1, mean = spline1$series[qqq], sd=residsSD) * new1)
  highFreq2[qqq] <- (spline1$series[qqq] * orig1)
  TSsur[qqq] <- spline1$curves[qqq] + noise2[qqq] + highFreq2[qqq]
}

cor(TSorig, TSsur)
cor(TSsur, TS1)

plotdf2 <- data.frame(Year=as.numeric(rownames(TNchron))[TSindex],
                      TTS=TS1+noise1,
                      Original=TS1,
                      Low_Freq=spline1$curves,
                      High_Freq=highFreq2,
                      Noise=noise2,
                      Synthetic=TSsur-1)

plotdf2 <- melt(plotdf2, id.vars = "Year")

tail(plotdf)

ggplot(data = plotdf2, mapping = aes(x=Year, y=value)) + geom_line() +
  facet_grid(variable~.)

plotdf$data<-"MV Chronology"
plotdf2$data<-"Time Series"

plotdf3 <- rbind(plotdf, plotdf2)

plotdf3$variable <- factor(plotdf3$variable, levels = c("Original", "TTS", "Low_Freq", "High_Freq", "Noise", "TMVSC", "Synthetic"))

ggplot(data = plotdf3, mapping = aes(x=Year, y=value)) + geom_line() +
  facet_grid(variable~data, scales = "free_x") +
  theme_classic()

ggsave(filename = "C:/Users/dce72/Documents/GradSchool/Uncertainty/FigSyntheticChrons.png", width = 7, height = 6)

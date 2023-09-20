#clear workspace
rm(list=ls())
gc()
#load package(s)
require(dplR)
require(forecast)
require(ggplot2)
require(reshape2)
require(cowplot)


#load Tree Nob detrended ring width data
climDat1 <- read.csv("C:/Users/dce72/Documents/GradSchool/Uncertainty/SimpleBootTrials/LangaraSST.csv")
climDat1 <- climDat1[climDat1$Year < 2002,]
TNchron <- read.rwl("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\SimpleBootTrials\\TreeNobRCStrunc1.csv")
TNchron <- TNchron[as.numeric(rownames(TNchron))>1725,]
TNchron <- TNchron[,apply(TNchron, 2, function(x) sum(!is.na(x)))>0]
TNmvchron <- chron(TNchron)
cor(TNmvchron[as.numeric(rownames(TNmvchron)) %in% climDat1[,1],1], climDat1[climDat1[,1] %in% as.numeric(rownames(TNmvchron)),2])
rwi.stats(TNchron)

#Bootstrap Tree NOb with ME and Traditional Methods
source("C:/Users/dce72/Documents/GradSchool/Uncertainty/R/chronBoot.R")
bootTNme <- chronBoot(chron1=TNchron, numIts = 100, boot = "ME", envelope=0.9)
bootTNtrad <- chronBoot(chron1=TNchron, numIts = 100, boot = "trad", envelope=0.9)

#Combine all data in a single data frame
start1 <- max(c(min(as.numeric(rownames(bootTNme$confInt))),min(climDat1[,1])))
end1 <- min(c(max(as.numeric(rownames(bootTNme$confInt))),max(climDat1[,1])))
reconDat1 <- data.frame("Year" = as.numeric(rownames(bootTNme$confInt)),
                        "Target" = rep(NA, length(bootTNme$confInt$X2)))
reconDat1[reconDat1[,1] %in% climDat1[,1], 2] <- climDat1[climDat1[,1] %in% reconDat1[,1],2]
reconDat1 <- reconDat1[reconDat1$Year %in% start1:end1,]
reconDat2 <- cbind(reconDat1[reconDat1$Year %in% as.numeric(rownames(bootTNme$confInt)),],
                   bootTNme$confInt[as.numeric(rownames(bootTNme$confInt)) %in% reconDat1$Year,])
reconDat2 <- cbind(reconDat2,
                   bootTNtrad$confInt[as.numeric(rownames(bootTNtrad$confInt)) %in% reconDat1$Year,c(1,3)])

#Set testing intervals
overlap1 <- reconDat2[complete.cases(reconDat2),1:3]
possibleIts1 <- length(overlap1[,1])
setAsideInterval <- 1:10
setAsideLen <- length(setAsideInterval)

calibIntervals <- data.frame(matrix(ncol = 3, nrow = (62*62), data=NA))
totalIndex <- 1:62
for (fff in 1:52){
  thisIndex <- totalIndex + (62 * (fff-1))
  outsideTheAside <- thisIndex[11:length(thisIndex)]
  insideTheAside <- thisIndex[1:10]
  possibleStarts <- c(outsideTheAside,outsideTheAside)
  verif1 <- possibleStarts[(fff+26):(fff+51)]
  calib1 <- possibleStarts[fff:(fff+25)]
  calibIntervals[outsideTheAside,1] <- 1950:2001
  calibIntervals[verif1,2] <- "verif"
  calibIntervals[calib1,2] <- "calib"
  calibIntervals[thisIndex,3] <- fff
  calibIntervals[insideTheAside,1] <- 1940:1949
  calibIntervals[insideTheAside,2] <- "aside"
}

pal2 <- c("#cc8b86","#508484","#f3a712","#cfd11a")

#calibIntervals <- calibIntervals[complete.cases(calibIntervals),]
p111 <- ggplot(data=calibIntervals, mapping = aes(x=X1, fill=X2)) +
  scale_fill_manual(values = pal2)+
  geom_bar(width=1) +
  facet_grid(X3~.) +
  theme_light()+
  scale_x_continuous(limits = c(1940,2001))+
  xlab("Year")+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none",
        strip.background.x = element_blank(),
        strip.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        panel.spacing = unit(0, "lines"),
        panel.border = element_blank()
        )
p111

calibIntervals2 <- calibIntervals[complete.cases(calibIntervals),]
calibIntervalsAll <- calibIntervals2
calibIntervalsAll$SetAside <- 1

for (k in 1:62){
  for (i in unique(calibIntervals2$X3)){
    currentRow <- calibIntervals2[calibIntervals2$X3 == i,]
    currentRow$X2 <- c(currentRow$X2[length(currentRow$X2)], currentRow$X2[1:(length(currentRow$X2)-1)])
    calibIntervals2$X2[calibIntervals2$X3 == i] <- currentRow$X2
  }
  calibIntervals2$SetAside <- k
  calibIntervalsAll <- rbind(calibIntervalsAll, calibIntervals2)
}



plotNum <- c(1,2,3,59, 60,61)
for (i in plotNum) {
  assign(paste("plot", i, sep = ""),
         ggplot(data=calibIntervalsAll[calibIntervalsAll$SetAside==i,], mapping = aes(x=X1, fill=X2)) +
           scale_fill_manual(values = pal2)+
           geom_bar(width=1) +
           facet_grid(X3~.) +
           theme_light()+
           #scale_x_continuous(limits = c(1940,2001))+
           xlab("Year")+
           theme(axis.title = element_blank(),
                 axis.text = element_blank(),
                 legend.position = "none",
                 strip.background.x = element_blank(),
                 strip.text = element_blank(),
                 axis.ticks = element_blank(),
                 axis.line = element_blank(),
                 panel.grid = element_blank(),
                 panel.spacing = unit(0, "lines"),
                 panel.border = element_blank()
           ))
}
plot61 <- plot61 + theme(axis.text.x = element_text(),
                         axis.title.x = element_text(),
                         axis.ticks = element_line())
p23498 <- plot_grid(p111, plot1, plot2, plot59, plot60, plot61, ncol = 1, rel_heights = c(1,1,1,1,1,2.5))


p030 <- ggplot(data=calibIntervals[calibIntervals$X3==1,], mapping = aes(x=X1, y=1, fill=X2)) +
  geom_line(lwd=10) +
  facet_grid(X3~.) +
  theme_light()+
  scale_x_continuous(limits = c(1940,2001))+
  xlab("Year")+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        strip.background.x = element_blank(),
        strip.text = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        panel.spacing = unit(.1, "lines"),
        panel.border = element_blank()
  )
p031 <- ggplot(data=calibIntervals[calibIntervals$X3==12,], mapping = aes(x=X1, fill=X2)) +
  geom_bar(width=1) +
  facet_grid(X3~.) +
  theme_light()+
  scale_x_continuous(limits = c(1940,2001))+
  xlab("Year")+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        panel.spacing = unit(.1, "lines"),
        panel.border = element_blank()
  )

plot_grid(p030,p031, ncol = 1)

verif1 <- 37:62
calib1 <- 11:36
interestInterval <- c(calib1,verif1)
names(reconDat2)[3:7] <- c("ChronoLowME", "Chronology", "ChronoHighME","ChronoLowTrad", "ChronoHighTrad")

#Calibrate reconstruction
reconModel <- lm(Target ~ Chronology, data = reconDat2[calib1,])
reconModelLowME <- lm(Target ~ ChronoLowME, data = reconDat2[calib1,])
reconModelHighME <- lm(Target ~ ChronoHighME, data = reconDat2[calib1,])
reconModelLowTrad <- lm(Target ~ ChronoLowTrad, data = reconDat2[calib1,])
reconModelHighTrad <- lm(Target ~ ChronoHighTrad, data = reconDat2[calib1,])

#Build reconstruction in full interest interval (excluding set aside) from calib interval coefficients
reconDat2$reconstruction[interestInterval] <- reconModel$coefficients[1] + reconModel$coefficients[2] * reconDat2$Chronology[interestInterval]
reconDat2$reconLowME[interestInterval] <- reconModelLowME$coefficients[1] + reconModelLowME$coefficients[2] * reconDat2$ChronoLowME[interestInterval]
reconDat2$reconHighME[interestInterval] <- reconModelHighME$coefficients[1] + reconModelHighME$coefficients[2] * reconDat2$ChronoHighME[interestInterval]
reconDat2$reconLowTrad[interestInterval] <- reconModelLowTrad$coefficients[1] + reconModelLowTrad$coefficients[2] * reconDat2$ChronoLowME[interestInterval]
reconDat2$reconHighTrad[interestInterval] <- reconModelHighTrad$coefficients[1] + reconModelHighTrad$coefficients[2] * reconDat2$ChronoHighME[interestInterval]

#Name intervals for plotting
reconDat2$interval <- NA
reconDat2$interval[setAsideInterval] <- "Aside"
reconDat2$interval[calib1] <- "Calibration"
reconDat2$interval[verif1] <- "Verification"

#measure confidence intervals
# errorSet1 <- abs(reconDat2$Target[verif1] - reconDat2$reconstruction[verif1])
# trueRMSE1 <- interpIndex(errorSet1, 0.9)
# reconDat2$PImeasHigh <- NA
# reconDat2$PImeasLow <- NA
# reconDat2$PImeasHigh[calib1] <- reconDat2$reconstruction[calib1] + trueRMSE1
# reconDat2$PImeasLow[calib1] <- reconDat2$reconstruction[calib1] - trueRMSE1

#measure prediction intervals
errorSet <- abs(reconDat2$Target[verif1] - reconDat2$reconstruction[verif1])
trueRMSE <- as.numeric(quantile(errorSet, probs=0.9))
tValRMSE <- median(errorSet) + sd(errorSet)* qt(p=(0.1), df=length(errorSet), lower.tail = FALSE)
reconDat2$PImeasHigh[verif1] <- reconDat2$reconstruction[verif1] + trueRMSE
reconDat2$PImeasLow[verif1] <- reconDat2$reconstruction[verif1] - trueRMSE

#reconstruct the set aside interval, calibrate reconstruction using all but the set aside data
reconModel2 <- lm(Target ~ Chronology, data = reconDat2[interestInterval,])
reconModelLow2ME <- lm(Target ~ ChronoLowME, data = reconDat2[interestInterval,])
reconModelHigh2ME <- lm(Target ~ ChronoHighME, data = reconDat2[interestInterval,])
reconModelLow2Trad <- lm(Target ~ ChronoLowTrad, data = reconDat2[interestInterval,])
reconModelHigh2Trad <- lm(Target ~ ChronoHighTrad, data = reconDat2[interestInterval,])

reconDat2$reconstruction[setAsideInterval] <- reconModel2$coefficients[1] + reconModel2$coefficients[2] * reconDat2$Chronology[setAsideInterval]
reconDat2$reconLowME[setAsideInterval] <- reconModelHigh2ME$coefficients[1] + reconModelHigh2ME$coefficients[2] * reconDat2$ChronoLowME[setAsideInterval]
reconDat2$reconHighME[setAsideInterval] <- reconModelLow2ME$coefficients[1] + reconModelLow2ME$coefficients[2] * reconDat2$ChronoHighME[setAsideInterval]
reconDat2$reconLowTrad[setAsideInterval] <- reconModelHigh2Trad$coefficients[1] + reconModelHigh2Trad$coefficients[2] * reconDat2$ChronoLowTrad[setAsideInterval]
reconDat2$reconHighTrad[setAsideInterval] <- reconModelLow2Trad$coefficients[1] + reconModelLow2Trad$coefficients[2] * reconDat2$ChronoHighTrad[setAsideInterval]
target1 <- reconDat2$Target[setAsideInterval]

#Build and test prediction intervals in the set aside interval
reconDat2$pred1High <- NA
reconDat2$pred1Low <- NA
reconDat2$pred1High[setAsideInterval] <- reconDat2$reconstruction[setAsideInterval] + trueRMSE
reconDat2$pred1Low[setAsideInterval] <- reconDat2$reconstruction[setAsideInterval] - trueRMSE
(setAsideLen-sum(target1 > reconDat2$pred1High[setAsideInterval] | target1 < reconDat2$pred1Low[setAsideInterval]))/setAsideLen

reconDat2$pred2High <- NA
reconDat2$pred2Low <- NA
reconDat2$pred2High[setAsideInterval] <- reconDat2$reconstruction[setAsideInterval] + tValRMSE
reconDat2$pred2Low[setAsideInterval] <- reconDat2$reconstruction[setAsideInterval] - tValRMSE
(setAsideLen-sum(target1 > reconDat2$pred2High[setAsideInterval] | target1 < reconDat2$pred2Low[setAsideInterval]))/setAsideLen

reconDat2$pred3High <- NA
reconDat2$pred3Low <- NA
reconDat2$pred3High[setAsideInterval] <- reconDat2$reconHighME[setAsideInterval] + trueRMSE
reconDat2$pred3Low[setAsideInterval] <- reconDat2$reconLowME[setAsideInterval] - trueRMSE
(setAsideLen-sum(target1 > reconDat2$pred3High[setAsideInterval] | target1 < reconDat2$pred3Low[setAsideInterval]))/setAsideLen

reconDat2$pred4High <- NA
reconDat2$pred4Low <- NA
reconDat2$pred4High[setAsideInterval] <- reconDat2$reconHighME[setAsideInterval] + tValRMSE
reconDat2$pred4Low[setAsideInterval] <- reconDat2$reconLowME[setAsideInterval] - tValRMSE
(setAsideLen-sum(target1 > reconDat2$pred4High[setAsideInterval] | target1 < reconDat2$pred4Low[setAsideInterval]))/setAsideLen

reconDat2$pred5High <- NA
reconDat2$pred5Low <- NA
reconDat2$pred5High[setAsideInterval] <- reconDat2$reconHighTrad[setAsideInterval] + trueRMSE
reconDat2$pred5Low[setAsideInterval] <- reconDat2$reconLowTrad[setAsideInterval] - trueRMSE
(setAsideLen-sum(target1 > reconDat2$pred5High[setAsideInterval] | target1 < reconDat2$pred5Low[setAsideInterval]))/setAsideLen

reconDat2$pred6High <- NA
reconDat2$pred6Low <- NA
reconDat2$pred6High[setAsideInterval] <- reconDat2$reconHighTrad[setAsideInterval] + tValRMSE
reconDat2$pred6Low[setAsideInterval] <- reconDat2$reconLowTrad[setAsideInterval] - tValRMSE
(setAsideLen-sum(target1 > reconDat2$pred6High[setAsideInterval] | target1 < reconDat2$pred6Low[setAsideInterval]))/setAsideLen

names(reconDat2)
plotData1 <- melt(reconDat2[,c(1,8,13:27)], id.vars=c("Year", "interval"))
plotData2 <- melt(reconDat2[,c(1,2,13)], id.vars=c("Year", "interval"))

# unique(plotData1$variable)
# plotData1$colorVar<-NA
# plotData1$colorVar[plotData1$variable %in% c("reconstruc")] <- "recon"
# plotData1$colorVar[plotData1$variable %in% c("PImeasHigh", "PImeasLow")] <- "PIMeas"
# for (i in 1:6){
#   plotData1$colorVar[plotData1$variable %in% c(paste0("pred",i,"High"), paste0("pred",i,"High"))] <- "pred1"
# }

pal1 <- c("black","orange", "orange", "#058E3F", "#058E3F",
          "darkgreen", "darkgreen", "#058E3F", "#058E3F",
          "darkblue", "darkblue", "#88292F", "#88292F", "darkred", "darkred")

pal2 <- c(alpha("#cc8b86", .006),alpha("#508484", .005),alpha("#f3a712", .005),alpha("#cfd11a", .005))

unique(plotData1$variable)
plotData1$plot1 <- 1


p1 <- ggplot(data = plotData1, mapping = aes(x=Year, y=value, color=variable, group=variable, linetype=variable, lwd=variable)) +
  geom_rect(data = plotData1, aes(fill = interval),xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf)+
  geom_line(lwd=0.8) +
  scale_fill_manual(values = pal2)+
  geom_point(data=plotData2, mapping=aes(x=Year, y=value), inherit.aes = FALSE) +
  scale_linetype_manual(values=c(1,2,2,rep(1,12)))+
  scale_size_manual(values=c(1,rep(.6,14)))+
  scale_x_continuous(breaks = c(seq(1940,2000,10)))+
  theme_light()+
  ylab("SST (\u00B0C)")+
  scale_color_manual(values = pal1)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        strip.background.x = element_rect(fill = "white", color = "black"),
        strip.text.x = element_text(color = "black"),
        strip.background.y = element_rect(fill = "white", color="white"),
        strip.text.y = element_text(color = "white"))+
  facet_grid(plot1~interval, scales="free_x", space="free_x")



library(cowplot)

pp <- list(p1, p111, p23498)
plot_grid(plotlist=pp, ncol=1, align='h', axis = "b")



#ggsave(filename = "C:/Users/dce72/Documents/GradSchool/Uncertainty/Fig1_New_OCt22.png", width = 7.5, height = 6.5)

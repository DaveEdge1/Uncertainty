#clear workspace
rm(list=ls())
gc()
library(dplR)
library(ggplot2)
library(gridExtra)
library(ggbeeswarm)
library(reshape2)

#Get capture data
allCapture <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\captureData.RData")

#Real chronology
test1RealDat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\TreeNobTest2.RData")
test2RealDat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\ca646Test2.RData")
test4RealDat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\cana113test2.RData")

#simulation
test1Dat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\TreeNobSimTest2.RData")
test2Dat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\ca646SimTest2.RData")
test4Dat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\cana113SimTest2.RData")

newChronNums <- c(304:606)

#Chronologies
##Tree Nob
TNchron <- read.rwl("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\SimpleBootTrials\\TreeNobRCStrunc1.csv")
TNchron <- TNchron[as.numeric(rownames(TNchron))>1725,]
climDat1 <- read.csv("C:/Users/dce72/Documents/GradSchool/Uncertainty/SimpleBootTrials/LangaraSST.csv")
climDat1 <- climDat1[climDat1$Year < 2002,]

#Rock Springs Ranch
chron646 <- read.rwl("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\SimpleBootTrials\\ca646rwi.csv")
chron646mv <- chron(chron646)
climDat2 <- read.csv("C:/Users/dce72/Documents/GradSchool/Uncertainty/SimpleBootTrials/ca646PrecipTarget.csv")
climDat2 <- climDat2[,-1]
names(climDat2) <- c("Year", "Target")

#Arrowsmith Mountain
climDat4 <- read.csv("C:/Users/dce72/Documents/GradSchool/Uncertainty/SimpleBootTrials/canaTempTarget.csv")
climDat4 <- climDat4[,-1]
names(climDat4) <- c("Year", "Target")
chron113 <- read.rwl("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\SimpleBootTrials\\cana113.rwl")
chron113mv <- chron(chron113)

#Simulation sequence
simSeq <- seq(1,400,4)

#initiate list to capture data
all3SitesSimData <- list()

ChronNums <- c(c(4:103,1), c(104:203,2), c(204:303,3))

for (bi in 1:3){
  testNow <- list(test1Dat, test2Dat, test4Dat)[[bi]]
  testNowReal <- list(test1RealDat, test2RealDat, test4RealDat)[[bi]]
  siteID <- c("TN", "RSR", "AM")[bi]
  chronReal <- list(TNchron, chron646, chron113)[[bi]]
  RealMVchron <- chron(chronReal)
  climDatNow <- list(climDat1, climDat2, climDat4)[[bi]]

  #################################
  #number the chronos



  ########################
  #check regression assumptions
  ########################
  normAss <- list()
  AR1Ass <- list()
  homoskedAss <- list()


  for (i in simSeq){
    normAss[[i]] <- testNow[[i]]$regAssumpt[1]
    AR1Ass[[i]] <- testNow[[i]]$regAssumpt[2]
    homoskedAss[[i]] <- testNow[[i]]$regAssumpt[3]
  }
  normAssAll <- as.numeric(c(unlist(normAss), testNowReal[[1]]$regAssumpt[1]))
  AR1AssAll <- as.numeric(c(unlist(AR1Ass), testNowReal[[1]]$regAssumpt[2]))
  homoskedAssAll <- as.numeric(c(unlist(homoskedAss), testNowReal[[1]]$regAssumpt[3]))


  ########################
  #rbar
  ########################

  TnSynRWI <- list()
  for (i in simSeq){
    rwiStats1 <- rwi.stats(testNow[[i]]$synchron$synChron)
    TnSynRWI[[i]] <- rwiStats1$rbar.eff
    # print(siteID)
    # print(i)
    # print(rwiStats1)
  }
  rbarSim <- unlist(TnSynRWI)
  rbarAll <- c(rbarSim, rwi.stats(chronReal)$rbar.eff)

  ########################
  #Get AR1 values
  ########################
  allAR1 <- unlist(lapply(testNow[simSeq], function(x) x$synchron$synChronData$surChronAR1))
  allAR1 <- c(allAR1, acf(RealMVchron[,1], plot = FALSE)$acf[2])

  ########################
  #Get correlations to target
  ########################
  realCor <- cor(RealMVchron[as.numeric(rownames(RealMVchron)) %in% climDatNow[,1],1], climDatNow[climDatNow[,1] %in% as.numeric(rownames(RealMVchron)),2])
  allCors <- rep(NA, 100)
  for(i in 1:100){
    k = simSeq[i]
    testNow[[k]]$synchron$synChronData
    synchron1 <- chron(testNow[[k]]$synchron$synChron)
    allCors[i] <- cor(synchron1[as.numeric(rownames(synchron1)) %in% climDatNow[,1],1], climDatNow[climDatNow[,1] %in% as.numeric(rownames(synchron1)),2])
  }

  allCors <- c(allCors, realCor)

  ########################
  #Get mean sample depths
  ########################

  realSD <- mean(apply(chronReal[as.numeric(rownames(chronReal)) %in% climDatNow[,1],], 1, function(x) sum(!is.na(x))))
  allSD <- rep(NA, 100)
  for(i in 1:100){
    k=simSeq[i]
    synchron1 <- testNow[[k]]$synchron$synChron
    allSD[i] <- mean(apply(synchron1[as.numeric(rownames(synchron1)) %in% climDatNow[,1],], 1, function(x) sum(!is.na(x))))
  }
  allSD <- c(allSD, realSD)


  ########################
  #Format data
  ########################

  allData <- data.frame(
    "residAR1" = AR1AssAll,
    "residnormTest" = normAssAll,
    "residVariance" = homoskedAssAll,
    "rbar" = rbarAll,
    "AR1" = allAR1,
    "Cor" = allCors,
    "SD" = allSD,
    "siteID" = rep(siteID, 101)
  )
  allData$chron <- NA
  allData$chron[1:100] <- "sim"
  allData$chron[101] <- "chron"

  all3SitesSimData[[bi]] <- allData

}

plotData828 <- rbind(all3SitesSimData[[1]], all3SitesSimData[[2]], all3SitesSimData[[3]])

dim(plotData828)
plotData828$chronNum <- ChronNums

plot828melt <- reshape2::melt(plotData828, id.vars = c("siteID", "chron", "chronNum"))
plot828melt1 <- plot828melt

lowFidelitySimsFig3 <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\lowFidelitySimsFig3.RData")

lowFidelitySimsFig3 <- lowFidelitySimsFig3[lowFidelitySimsFig3$chron != "chron",]

plot828melt <- rbind(plot828melt, lowFidelitySimsFig3)

#saveRDS(plot828melt, "C:/Users/dce72/Documents/GradSchool/Uncertainty/ChronologyProperties2.RData")
###############################################################################
#resid AR1 plot

residAR1DatMelt <- plot828melt[plot828melt$variable == "residAR1",]
residAR1DatMelt$AR1 <- NA
residAR1DatMelt$AR1[residAR1DatMelt$value > 0.25] <- "fail"
residAR1DatMelt$AR1[residAR1DatMelt$value <= 0.25] <- "pass"

captureNames <- c(
  'AM' = "Arrowsmith Mountain",
  'RSR' = "Rock Springs Ranch",
  'TN' = "Tree Nob",
  'residAR1' = "AR1"
)

p001 <- ggplot(data = residAR1DatMelt, mapping = aes(y=value, x = siteID, shape=chron, color=AR1)) +
  geom_hline(yintercept = 0.25, lty=2, color=alpha("red", 0.5))+
  geom_quasirandom(alpha = 0.8, width = 0.5, mapping = aes(shape=chron, size=chron, color=AR1)) +
  scale_size_manual(values = c(3,.35))+
  scale_shape_manual(values = c(17,20))+
  #scale_alpha_manual(values = c(1,0.1))+
  theme_classic()+
  facet_grid(variable~siteID, scales = "free", space = "fixed", labeller = as_labeller(captureNames))+
  #scale_color_brewer(palette="Dark2")+
  scale_color_manual(values = c("red", "black"))+
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    text = element_text(size = 10, color = "black"),
    legend.title = element_blank(),
    legend.position = "none"
  )
p001
###############################################################################
#resid norm plot
unique(plot828melt$variable)
residNormDatMelt <- plot828melt[plot828melt$variable == "residnormTest",]
residNormDatMelt$Normality <- NA
residNormDatMelt$Normality[residNormDatMelt$value >= 0.05] <- "pass"
residNormDatMelt$Normality[residNormDatMelt$value < 0.05] <- "fail"

captureNames <- c(
  'AM' = "Arrowsmith Mountain",
  'RSR' = "Rock Springs Ranch",
  'TN' = "Tree Nob",
  'residnormTest' = "Normality"
)

p002 <- ggplot(data = residNormDatMelt, mapping = aes(y=log10(value), x = siteID, shape=chron, color=Normality)) +
  geom_hline(yintercept = log10(0.05), lty=2, color=alpha("red", 0.5))+
  geom_quasirandom(alpha = 0.8, width = 0.5, mapping = aes(shape=chron, size=chron, color=Normality)) +
  scale_size_manual(values = c(3,.35))+
  scale_shape_manual(values = c(17,20))+
  #scale_alpha_manual(values = c(1,0.1))+
  theme_classic()+
  facet_grid(variable~siteID, scales = "free", space = "fixed", labeller = as_labeller(captureNames))+
  #scale_color_brewer(palette="Dark2")+
  scale_color_manual(values = c("red", "black"))+
  ylab("AR1")+
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    text = element_text(size = 10, color = "black"),
    legend.title = element_blank(),
    legend.position = "none",
    strip.background.x = element_blank(),
    strip.text.x = element_blank(),
  )
p002
###############################################################################
#resid norm plot
unique(plot828melt$variable)
VarDataTest <- plot828melt[plot828melt$variable == "residVariance",]
VarDataTest$Homoscedastic <- NA
VarDataTest$Homoscedastic[VarDataTest$value >= 0.05] <- "pass"
VarDataTest$Homoscedastic[VarDataTest$value < 0.05] <- "fail"

captureNames <- c(
  'AM' = "Arrowsmith Mountain",
  'RSR' = "Rock Springs Ranch",
  'TN' = "Tree Nob",
  'residVariance' = "Homoscedastic"
)

p003 <- ggplot(data = VarDataTest, mapping = aes(y=log10(value), x = siteID, shape=chron, color=Homoscedastic)) +
  geom_hline(yintercept = log10(0.05), lty=2, color=alpha("red", 0.5))+
  geom_quasirandom(alpha = 0.8, width = 0.5, mapping = aes(shape=chron, size=chron, color=Homoscedastic)) +
  scale_size_manual(values = c(3,.35))+
  scale_shape_manual(values = c(17,20))+
  #scale_alpha_manual(values = c(1,0.1))+
  theme_classic()+
  facet_grid(variable~siteID, scales = "free", space = "fixed", labeller = as_labeller(captureNames))+
  #scale_color_brewer(palette="Dark2")+
  scale_color_manual(values = c("red", "black"))+
  ylab("AR1")+
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    text = element_text(size = 10, color = "black"),
    legend.title = element_blank(),
    legend.position = "none",
    strip.background.x = element_blank(),
    strip.text.x = element_blank(),
  )
p003
###############################################################################
#all regression tests
ggpubr::ggarrange(p001, p002, p003, ncol = 1, common.legend = TRUE, legend = "bottom", heights = c(.25,.2,.2))
#ggsave(filename = "C:/Users/dce72/Documents/GradSchool/Uncertainty/Fig4.png", width = 7, height = 4)

###############################################################################
#Mark failed tests in chronology properties plot
allTestDat <- cbind(residAR1DatMelt$AR1, paste0(residAR1DatMelt$siteID, residAR1DatMelt$chron),
                    residNormDatMelt$Normality, paste0(residNormDatMelt$siteID, residNormDatMelt$chron),
                    VarDataTest$Homoscedastic, paste0(VarDataTest$siteID, VarDataTest$chron))


sum(allTestDat[,2] != allTestDat[,4])
sum(allTestDat[,2] != allTestDat[,6])

allTestDat <- cbind(allTestDat, apply(allTestDat[,c(1,3,5)], 1, function(x) length(grep("fail", x))>0))
sum(as.logical(allTestDat[,7]))

allTestDat1 <- allTestDat[1:303,7]
allTestDat2 <- allTestDat[304:603,7]

plot828melt1 <- cbind(plot828melt1, allTestDat1)
colnames(plot828melt1)[5] <- "regTest"

lowFidelitySimsFig3 <- cbind(lowFidelitySimsFig3, allTestDat2)
colnames(lowFidelitySimsFig3)[5] <- "regTest"

lowFidelitySimsFig3 <- lowFidelitySimsFig3[lowFidelitySimsFig3$chron != "chron",]

plot828melt <- rbind(plot828melt1, lowFidelitySimsFig3)




plot828melt <- plot828melt[plot828melt$variable != "residAR1",]
plot828melt <- plot828melt[plot828melt$variable != "residVariance",]
plot828melt <- plot828melt[plot828melt$variable != "residnormTest",]




###############################################################################
#plot chronology properties
captureNames <- c(
  'residAR1' = "residAR1",
  'residnormTest' = "residNormTest",
  'residVariance' = "residHoSced",
  'AM' = "Arrowsmith Mountain",
  'RSR' = "Rock Springs Ranch",
  'TN' = "Tree Nob",
  'rbar' = "rbar",
  "AR1" = "AR1",
  "Cor" = "Target Cor",
  "SD" = "Sample Depth"
)
pal1 <- c("#cc8b86","#54494b","#508484","#f3a712","#cfd11a")

plot828melt <- plot828melt[order(plot828melt$chron, decreasing = TRUE),]

ggplot(data = plot828melt, mapping = aes(y=value, x = siteID, shape=chron, color=variable)) +
  geom_quasirandom(alpha = 0.8, width = 0.5, mapping = aes(shape=chron, size=chron, color=variable)) +
  scale_size_manual(values = c(3,1))+
  scale_shape_manual(values = c(17,20))+
  scale_color_manual(values = c("black","red"))+
  theme_classic()+
  facet_grid(variable~siteID, scales = "free", space = "fixed", labeller = as_labeller(captureNames))+
  #scale_color_brewer(palette="Dark2")+
  scale_color_manual(values = pal1)+
  theme(
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    text = element_text(size = 10, color = "black"),
    legend.title = element_blank(),
    legend.position = "none"
  )

#ggsave(filename = "C:/Users/dce72/Documents/GradSchool/Uncertainty/Fig3.png", width = 7, height = 5)

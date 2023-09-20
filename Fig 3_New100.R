#clear workspace
rm(list=ls())
gc()
library(dplR)
library(ggplot2)
library(gridExtra)
library(ggbeeswarm)

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

ChronNums <- c(c(307:406,304), c(407:506,305), c(507:606,306))

for (bi in 1:3){
  testNow <- list(test1Dat, test2Dat, test4Dat)[[bi]]
  testNowReal <- list(test1RealDat, test2RealDat, test4RealDat)[[bi]]
  siteID <- c("TN", "RSR", "AM")[bi]
  chronReal <- list(TNchron, chron646, chron113)[[bi]]
  RealMVchron <- chron(chronReal)
  climDatNow <- list(climDat1, climDat2, climDat4)[[bi]]

  ########################
  #check regression assumptions
  ########################
  normAss <- list()
  AR1Ass <- list()
  homoskedAss <- list()


  for (i in simSeq){
    normAss[[i]] <- testNow[[i]]$regAssumpt[1]
    #AR1Ass[[i]] <- testNow[[i]]$regAssumpt[2]
    homoskedAss[[i]] <- testNow[[i]]$regAssumpt[3]
  }
  normAssAll <- as.numeric(c(unlist(normAss), testNowReal[[1]]$regAssumpt[1]))
  #AR1AssAll <- as.numeric(c(unlist(AR1Ass), testNowReal[[1]]$regAssumpt[2]))
  homoskedAssAll <- as.numeric(c(unlist(homoskedAss), testNowReal[[1]]$regAssumpt[3]))

  for (i in simSeq){
    synchron1 <- testNow[[i]]$synchron$synChron
    synchron1MV <- chron(synchron1)
    mod1 <- lm(synchron1MV[as.numeric(rownames(synchron1MV)) %in% climDatNow[,1],1] ~
                 climDatNow[climDatNow[,1] %in% as.numeric(rownames(synchron1MV)),2])
    AR1Ass[[i]] <- lmtest::dwtest(mod1)$p.value
  }

  mod1 <- lm(RealMVchron[as.numeric(rownames(RealMVchron)) %in% climDatNow[,1],1] ~
               climDatNow[climDatNow[,1] %in% as.numeric(rownames(RealMVchron)),2])
  AR1AssReal <- lmtest::dwtest(mod1)$p.value


  AR1AssAll <- as.numeric(c(unlist(AR1Ass), AR1AssReal))

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
head(plot828melt)

#saveRDS(plot828melt, "C:/Users/dce72/Documents/GradSchool/Uncertainty/ChronologyPropertiesNew100.RData")
#saveRDS(plot828melt, file = "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\lowFidelitySimsFig3.RData")


residAR1DatMelt <- plot828melt[plot828melt$variable == "residAR1",]
residAR1DatMelt$AR1 <- NA
residAR1DatMelt$AR1[residAR1DatMelt$value < 0.05] <- "Reject"
residAR1DatMelt$AR1[residAR1DatMelt$value >= 0.05] <- "Accept"
residAR1DatMelt$chron[residAR1DatMelt$chron == "chron"] <- "Original"
residAR1DatMelt$chron[residAR1DatMelt$chron == "sim"] <- "Synthetic"
library(scales)

captureNames <- c(
  'AM' = "Arrowsmith Mountain",
  'RSR' = "Rock Springs Ranch",
  'TN' = "Tree Nob",
  'residAR1' = "No AR1"
)
sum(residAR1DatMelt$AR1[residAR1DatMelt$siteID == "AM"] == "Reject")
p001 <- ggplot(data = residAR1DatMelt, mapping = aes(y=value, x = siteID, shape=chron, color=AR1)) +
  geom_hline(yintercept = 0.05, lty=2, color=alpha("red", 0.5))+
  geom_quasirandom(alpha = 0.8, width = 0.5, mapping = aes(shape=chron, size=chron, color=AR1)) +
  scale_size_manual(values = c(3,.35))+
  scale_shape_manual(values = c(17,20))+
  #scale_alpha_manual(values = c(1,0.1))+
  theme_classic()+
  facet_grid(variable~siteID, scales = "free", space = "fixed", labeller = as_labeller(captureNames))+
  #scale_color_brewer(palette="Dark2")+
  scale_color_manual(values = c("black", "red"))+
  scale_y_continuous(trans = log10_trans(), breaks = c(0.000001, 0.0001, 0.01, 1),
                     labels = c("1E-6", "1E-4", "1E-2", "1"))+
  ylab("D-W test (p-value)")+
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    text = element_text(size = 10, color = "black"),
    legend.title = element_blank(),
    legend.position = "none",
    axis.ticks.x = element_blank()
  )
p001
###############################################################################
#resid norm plot
unique(plot828melt$variable)
residNormDatMelt <- plot828melt[plot828melt$variable == "residnormTest",]
residNormDatMelt$Normality <- NA
residNormDatMelt$Normality[residNormDatMelt$value >= 0.05] <- "Accept"
residNormDatMelt$Normality[residNormDatMelt$value < 0.05] <- "Reject"
residNormDatMelt$chron[residNormDatMelt$chron == "chron"] <- "Original"
residNormDatMelt$chron[residNormDatMelt$chron == "sim"] <- "Synthetic"
captureNames <- c(
  'AM' = "Arrowsmith Mountain",
  'RSR' = "Rock Springs Ranch",
  'TN' = "Tree Nob",
  'residnormTest' = "Normal"
)

p002 <- ggplot(data = residNormDatMelt, mapping = aes(y=value, x = siteID, shape=chron, color=Normality)) +
  geom_hline(yintercept = 0.05, lty=2, color=alpha("red", 0.5))+
  geom_quasirandom(alpha = 0.8, width = 0.5, mapping = aes(shape=chron, size=chron, color=Normality)) +
  scale_size_manual(values = c(3,.35))+
  scale_shape_manual(values = c(17,20))+
  #scale_alpha_manual(values = c(1,0.1))+
  theme_classic()+
  facet_grid(variable~siteID, scales = "free", space = "fixed", labeller = as_labeller(captureNames))+
  #scale_color_brewer(palette="Dark2")+
  scale_color_manual(values = c("black", "red"))+
  scale_y_continuous(trans = log10_trans(), breaks = c(0.001, 0.01, 0.1, 1),
                     labels = c("1E-3", "1E-2", "1E-1", "1"))+
  ylab("SW test (p-value)")+
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    text = element_text(size = 10, color = "black"),
    legend.title = element_blank(),
    legend.position = "none",
    strip.background.x = element_blank(),
    strip.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
p002
###############################################################################
#resid norm plot
unique(plot828melt$variable)
VarDataTest <- plot828melt[plot828melt$variable == "residVariance",]
VarDataTest$Homoscedastic <- NA
VarDataTest$Homoscedastic[VarDataTest$value >= 0.05] <- "Accept"
VarDataTest$Homoscedastic[VarDataTest$value < 0.05] <- "Reject"
VarDataTest$chron[VarDataTest$chron == "chron"] <- "Original"
VarDataTest$chron[VarDataTest$chron == "sim"] <- "Synthetic"
captureNames <- c(
  'AM' = "Arrowsmith Mountain",
  'RSR' = "Rock Springs Ranch",
  'TN' = "Tree Nob",
  'residVariance' = "Homoscedastic"
)

p003 <- ggplot(data = VarDataTest, mapping = aes(y=value, x = siteID, shape=chron, color=Homoscedastic)) +
  geom_hline(yintercept = 0.05, lty=2, color=alpha("red", 0.5))+
  geom_quasirandom(alpha = 0.8, width = 0.5, mapping = aes(shape=chron, size=chron, color=Homoscedastic)) +
  scale_size_manual(values = c(3,.35))+
  scale_shape_manual(values = c(17,20))+
  #scale_alpha_manual(values = c(1,0.1))+
  theme_classic()+
  facet_grid(variable~siteID, scales = "free", space = "fixed", labeller = as_labeller(captureNames))+
  #scale_color_brewer(palette="Dark2")+
  scale_color_manual(values = c("black", "red"))+
  scale_y_continuous(trans = log10_trans(), breaks = c(0.001, 0.01, 0.1, 1),
                     labels = c("1E-3", "1E-2", "1E-1", "1"))+
  ylab("G-Q test (p-value)")+
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    text = element_text(size = 10, color = "black"),
    legend.title = element_blank(),
    legend.position = "none",
    strip.background.x = element_blank(),
    strip.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
p003
###############################################################################
#all regression tests
ggpubr::ggarrange(p001, p002, p003, ncol = 1, common.legend = TRUE, legend = "bottom", heights = c(.25,.2,.2))
#ggsave(filename = "C:/Users/dce72/Documents/GradSchool/Uncertainty/Fig4_New100.png", width = 7, height = 5)

###############################################################################
#Mark failed tests in chronology properties plot
allTestDat <- cbind(residAR1DatMelt$AR1, paste0(residAR1DatMelt$siteID, residAR1DatMelt$chron),
                    residNormDatMelt$Normality, paste0(residNormDatMelt$siteID, residNormDatMelt$chron),
                    VarDataTest$Homoscedastic, paste0(VarDataTest$siteID, VarDataTest$chron))

head(allTestDat)
dim(allTestDat)
dim(plotData828)
head(plotData828)
#make sure the order of the columns matches
sum(allTestDat[,2] != allTestDat[,4])
sum(allTestDat[,2] != allTestDat[,6])

allTestDat <- cbind(allTestDat, apply(allTestDat[,c(1,3,5)], 1, function(x) length(grep("Reject", x))>0))
sum(as.logical(allTestDat[,7]))


plot752 <- cbind(plotData828, allTestDat[,7])
head(plot752)
names(plot752)[11] <- "RegTest"
#colnames(plot828melt)[5] <- "regTest"

#lowFidelitySimsFig3 <- cbind(lowFidelitySimsFig3, allTestDat2)
#colnames(lowFidelitySimsFig3)[5] <- "regTest"

#saveRDS(plot752, "C:/Users/dce72/Documents/GradSchool/Uncertainty/RegAssumptions_New100.RData")

#plot828melt <- rbind(plot828melt1, lowFidelitySimsFig3)
plot752melt <- melt(plot752)



plot752melt <- plot752melt[plot752melt$variable != "residAR1",]
plot752melt <- plot752melt[plot752melt$variable != "residVariance",]
plot752melt <- plot752melt[plot752melt$variable != "residnormTest",]
plot752melt <- plot752melt[plot752melt$variable != "chronNum",]


head(plot752melt)

###############################################################################
#plot chronology properties
captureNames <- c(
  'residAR1' = "residAR1",
  'residnormTest' = "residNormTest",
  'residVariance' = "residHoSced",
  'AM' = "Arrowsmith Mountain",
  'RSR' = "Rock Springs Ranch",
  'TN' = "Tree Nob",
  'rbar' = "RBAR",
  "AR1" = "AR1",
  "Cor" = "Target Cor",
  "SD" = "Sample Depth"
)
pal1 <- c("#cc8b86","#54494b","#508484","#f3a712","#cfd11a")

plot752melt <- plot752melt[order(plot752melt$chron, decreasing = TRUE),]

p00001 <- ggplot(data = plot752melt[plot752melt$variable == "SD",], mapping = aes(y=value, x = siteID, shape=chron, color=RegTest)) +
  geom_quasirandom(alpha = 0.8, width = 0.5, mapping = aes(shape=chron, size=chron, color=RegTest)) +
  scale_size_manual(values = c(3,1))+
  scale_shape_manual(values = c(17,20))+
  theme_classic()+
  facet_grid(variable~siteID, scales = "free", space = "fixed", labeller = as_labeller(captureNames))+
  #scale_color_brewer(palette="Dark2")+
  scale_color_manual(values = c(pal1[4], "grey90"))+
  ylab("Count")+
  theme(
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    strip.background.x = element_blank(),
    strip.text.x = element_blank(),
    text = element_text(size = 10, color = "black"),
    legend.title = element_blank(),
    legend.position = "none"
  )
p00002 <- ggplot(data = plot752melt[plot752melt$variable == "Cor",], mapping = aes(y=value, x = siteID, shape=chron, color=RegTest)) +
  geom_quasirandom(alpha = 0.8, width = 0.5, mapping = aes(shape=chron, size=chron, color=RegTest)) +
  scale_size_manual(values = c(3,1))+
  scale_shape_manual(values = c(17,20))+
  theme_classic()+
  facet_grid(variable~siteID, scales = "free", space = "fixed", labeller = as_labeller(captureNames))+
  #scale_color_brewer(palette="Dark2")+
  scale_color_manual(values = c(pal1[3], "grey90"))+
  ylab("Correlation")+
  theme(
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    strip.background.x = element_blank(),
    strip.text.x = element_blank(),
    text = element_text(size = 10, color = "black"),
    legend.title = element_blank(),
    legend.position = "none"
  )

p00003 <- ggplot(data = plot752melt[plot752melt$variable == "AR1",], mapping = aes(y=value, x = siteID, shape=chron, color=RegTest)) +
  geom_quasirandom(alpha = 0.8, width = 0.5, mapping = aes(shape=chron, size=chron, color=RegTest)) +
  scale_size_manual(values = c(3,1))+
  scale_shape_manual(values = c(17,20))+
  theme_classic()+
  facet_grid(variable~siteID, scales = "free", space = "fixed", labeller = as_labeller(captureNames))+
  #scale_color_brewer(palette="Dark2")+
  scale_color_manual(values = c(pal1[2], "grey90"))+
  ylab("Correlation")+
  theme(
    axis.title.x = element_blank(),
    strip.background.x = element_blank(),
    strip.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(size = 10, color = "black"),
    legend.title = element_blank(),
    legend.position = "none"
  )

p00004 <- ggplot(data = plot752melt[plot752melt$variable == "rbar",], mapping = aes(y=value, x = siteID, shape=chron, color=RegTest)) +
  geom_quasirandom(alpha = 0.8, width = 0.5, mapping = aes(shape=chron, size=chron, color=RegTest)) +
  scale_size_manual(values = c(3,1))+
  scale_shape_manual(values = c(17,20))+
  theme_classic()+
  facet_grid(variable~siteID, scales = "free", space = "fixed", labeller = as_labeller(captureNames))+
  #scale_color_brewer(palette="Dark2")+
  scale_color_manual(values = c(pal1[1], "grey90"))+
  ylab("Correlation")+
  theme(
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(size = 10, color = "black"),
    legend.title = element_blank(),
    legend.position = "none"
  )

plot752melt$chron[plot752melt$chron == "chron"] <- "Original"
plot752melt$chron[plot752melt$chron == "sim"] <- "Synthetic"
plot752melt$RegTest[plot752melt$RegTest == "FALSE"] <- "Accept"
plot752melt$RegTest[plot752melt$RegTest == "TRUE"] <- "Reject"

p00005 <- ggplot(data = plot752melt, mapping = aes(y=value, x = siteID, shape=chron, color=RegTest)) +
  geom_quasirandom(alpha = 0.8, width = 0.5, mapping = aes(shape=chron, size=chron, color=RegTest)) +
  scale_size_manual(values = c(3,1))+
  scale_shape_manual(values = c(17,20))+
  scale_color_manual(values = c("black","grey90"))+
  theme_classic()+
  facet_grid(variable~siteID, scales = "free", space = "fixed", labeller = as_labeller(captureNames))+
  #scale_color_brewer(palette="Dark2")+
  #scale_color_manual(values = pal1)+
  ylab("Correlation")+
  theme(
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(size = 10, color = "black"),
    legend.title = element_blank(),
    legend.position = "none"
  )

ggpubr::ggarrange(p00005, p00004, p00001, p00002, p00003, ncol = 1, common.legend = TRUE, legend = "bottom", heights = c(.00001,.25,.2,.2,.2))
#ggsave(filename = "C:/Users/dce72/Documents/GradSchool/Uncertainty/Fig3_New100.png", width = 7, height = 5)

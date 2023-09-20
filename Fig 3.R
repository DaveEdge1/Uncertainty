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
test1RealDat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\TreeNobTest.RData")
test2RealDat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\ca646Test.RData")
test4RealDat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\cana113test.RData")

#simulation
test1Dat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\TreeNobSimTest.RData")
test2Dat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\ca646SimTest.RData")
test4Dat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\cana113SimTest.RData")

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
head(plot828melt)


#saveRDS(plot828melt, file = "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\lowFidelitySimsFig3.RData")

captureNames <- c(
  'residAR1' = "residAR1",
  'residnormTest' = "residNormTest",
  'residVariance' = "residVariance",
  'AM' = "Arrowsmith Mountain",
  'RSR' = "Rock Springs Ranch",
  'TN' = "Tree Nob",
  'rbar' = "rbar",
  "AR1" = "AR1",
  "Cor" = "Target Cor",
  "SD" = "Sample Depth"
)

ggplot(data = plot828melt, mapping = aes(y=value, x = siteID, shape=chron, color=variable)) +
  geom_quasirandom(alpha = 0.8, width = 0.2, mapping = aes(shape=chron, size=chron, color=variable)) +
  scale_size_manual(values = c(3,1))+
  scale_shape_manual(values = c(17,20))+
  theme_classic()+
  facet_grid(variable~siteID, scales = "free", space = "fixed", labeller = as_labeller(captureNames))+
  scale_color_brewer(palette="Dark2")+
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    text = element_text(size = 10, color = "black"),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

#ggsave(filename = "C:/Users/dce72/Documents/GradSchool/Uncertainty/Fig3.png", width = 7, height = 4.5)


TNFiftyCapture <- allCapture[allCapture$variable == "TN1" & allCapture$capture == "Fifty",]
TNFifty <- cbind(all3SitesSimData[[1]], TNFiftyCapture)
cor(TNFifty[,c(1,2,3,4,8)])


uniqueVars <- levels(allCapture$variable)

scatterData <- list()
for (i in uniqueVars){
  TNFiftyCapture <- allCapture[allCapture$variable == i,]
  TNFifty <- cbind(all3SitesSimData[[1]], TNFiftyCapture)
  # cat("\n", i, "\n")
  a1 <- unique(allCapture$capture[allCapture$variable == i])
  a2 <- unique(allCapture$boot[allCapture$variable == i])
  a3 <- unique(allCapture$Err[allCapture$variable == i])
  # print(cor(TNFifty[,c(1,2,3,4,8)]))
  scatterData[[i]]$data <- TNFifty[,c(1,2,3,4,8)]
  scatterData[[i]]$run <- c(i, a1, a2, a3)
}


plotData57 <- reshape2::melt(scatterData[[1]])




allScatterPlots <- list()

runCount <- 0
siteCount <- 0
dataVars <- names(scatterData[[1]]$data)
for (i in 1:3){
  site1 <- unique(allCapture$site)[i]
  for (j in 1:9){
    siteCount <- siteCount + 1
    runCount <- runCount + 1
    allScatterPlots[[runCount]] <- cor(scatterData[[siteCount]]$data$Percent_Capture, scatterData[[siteCount]]$data$rbar)
    # allScatterPlots[[runCount]] <- ggplot(data = scatterData[[siteCount]]$data, mapping = aes(x=Percent_Capture, y=rbar)) + geom_point() +
    #   geom_smooth(method='lm', formula= y~x) +
    #   theme(axis.title = element_blank(), axis.text = element_blank())
    runCount <- runCount + 1
    allScatterPlots[[runCount]] <- cor(scatterData[[siteCount]]$data$Percent_Capture, scatterData[[siteCount]]$data$Cor)
    # allScatterPlots[[runCount]] <- ggplot(data = scatterData[[siteCount]]$data, mapping = aes(x=Percent_Capture, y=Cor)) + geom_point() +
    #   theme(axis.title = element_blank(), axis.text = element_blank())
    runCount <- runCount + 1
    allScatterPlots[[runCount]] <- cor(scatterData[[siteCount]]$data$Percent_Capture, scatterData[[siteCount]]$data$AR1)
    # allScatterPlots[[runCount]] <- ggplot(data = scatterData[[siteCount]]$data, mapping = aes(x=Percent_Capture, y=AR1)) + geom_point() +
    #   theme(axis.title = element_blank(), axis.text = element_blank())
    runCount <- runCount + 1
    allScatterPlots[[runCount]] <- cor(scatterData[[siteCount]]$data$Percent_Capture, scatterData[[siteCount]]$data$SD)
    # allScatterPlots[[runCount]] <- ggplot(data = scatterData[[siteCount]]$data, mapping = aes(x=Percent_Capture, y=SD)) + geom_point() +
    #   theme(axis.title = element_blank(), axis.text = element_blank())
    cat(runCount, siteCount, scatterVar, "\n")
  }
}



length(allScatterPlots)

#gridExtra::grid.arrange(allScatterPlots)
allScatterPlots[[107]]
scatterCors <- unlist(allScatterPlots)
TreeNobCors <- data.frame("rbar" = scatterCors[seq(1,36,4)],
                          "Target_Cor" = scatterCors[seq(2,36,4)],
                          "AR1" = scatterCors[seq(3,36,4)],
                          "Sample_Depth" = scatterCors[seq(4,36,4)]
                          )
RockSpringsRanchCors <- data.frame("rbar" = scatterCors[seq(37,72,4)],
                          "Target_Cor" = scatterCors[seq(38,72,4)],
                          "AR1" = scatterCors[seq(39,72,4)],
                          "Sample_Depth" = scatterCors[seq(40,72,4)]
)
ArrowsmithMountainCors <- data.frame("rbar" = scatterCors[seq(73,108,4)],
                                   "Target_Cor" = scatterCors[seq(74,108,4)],
                                   "AR1" = scatterCors[seq(75,108,4)],
                                   "Sample_Depth" = scatterCors[seq(76,108,4)]
)


colMeans(TreeNobCors)
colMeans(RockSpringsRanchCors)
colMeans(ArrowsmithMountainCors)


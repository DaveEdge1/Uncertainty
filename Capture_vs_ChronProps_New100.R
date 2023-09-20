#clear workspace
rm(list=ls())
gc()
library(dplR)
library(ggplot2)
library(gridExtra)
library(ggbeeswarm)
library(reshape2)


#allChronProps <- readRDS("C:/Users/dce72/Documents/GradSchool/Uncertainty/ChronologyProperties.RData")
#allChronProps <- readRDS("C:/Users/dce72/Documents/GradSchool/Uncertainty/ChronologyProperties2.RData")
allChronProps <- readRDS("C:/Users/dce72/Documents/GradSchool/Uncertainty/ChronologyPropertiesNew100.RData")
dim(allChronProps)
#allChronProps[allChronProps$chronNum %in% 1:3,] <- NA
# allChronProps$chronNum[allChronProps$chronNum == 1] <- 304
# allChronProps$chronNum[allChronProps$chronNum == 2] <- 305
# allChronProps$chronNum[allChronProps$chronNum == 3] <- 306
#allChronProps <- allChronProps[allChronProps$chronNum > 303,]
allChronProps <- allChronProps[order(allChronProps$chronNum),]
#allChronProps <- allChronProps[allChronProps$chronNum %in% 304:606,]
dim(allChronProps)
#allCapture <-readRDS("C:/Users/dce72/Documents/GradSchool/Uncertainty/CaptureDatNewOct25.RData")
allCapture <- readRDS("C:/Users/dce72/Documents/GradSchool/Uncertainty/CaptureDatNew100.RData")
#allCapture <- allCapture[allCapture$chronNum > 303,]
allCapture <- allCapture[order(allCapture$chronNums),]
head(allCapture)
dim(allCapture)
#allCapture <- allCapture[allCapture$chronNum %in% 304:606,]
#allCapture <-readRDS("C:/Users/dce72/Documents/GradSchool/Uncertainty/CaptureDat.RData")
allCapture$Err <- paste0(allCapture$VE, allCapture$IC)
names(allCapture)[4] <- "boot"

names(allChronProps)[4] <- "chronProp"

allCapture$siteID <- NA
allCapture$siteID[allCapture$Site == "Tree Nob"] <- "TN"
allCapture$siteID[allCapture$Site == "Rock Springs Ranch"] <- "RSR"
allCapture$siteID[allCapture$Site == "Arrowsmith Mountain"] <- "AM"


allCapture$Chrono[allCapture$Chrono == "Synthetic"] <- "sim"
allCapture$Chrono[allCapture$Chrono == "Original"] <- "chron"

names(allCapture)[2] <- "PImethod"


rbarDat <- allChronProps[allChronProps$chronProp == "rbar",]
AR1Dat <- allChronProps[allChronProps$chronProp == "AR1",]
corDat <- allChronProps[allChronProps$chronProp == "Cor",]
sdDat <- allChronProps[allChronProps$chronProp == "SD",]



#rm(allChronProps)

capturePropertyCors <- data.frame(matrix(nrow=144, ncol=5, data=NA))
colnames(capturePropertyCors) <- c("Property", "Err_Method", "Site", "Boot", "Correlation")

chronPropNames <- c("rbar", "AR1", "Cor", "SD")

siteIDs <- unique(allCapture$siteID)
Errs <- unique(allCapture$Err)
boots <- unique(allCapture$boot)
propVals <- list(rbarDat, AR1Dat, corDat, sdDat)
propNames <- c("rbarDat", "AR1Dat", "corDat", "sdDat")
rowCt <- 0
for (m in 1:4){
  for (j in 1:3){
    for (k in 1:4){
      for (i in 1:3){
        rowCt <- rowCt + 1
        TNME90E <- allCapture[allCapture$boot == as.character(boots)[j] &
                                allCapture$Err == as.character(Errs)[k] &
                                allCapture$siteID == as.character(siteIDs)[i],]

        cor1 <- cor(propVals[[m]]$value[propVals[[m]]$siteID == as.character(siteIDs)[i]], TNME90E$value)

        capturePropertyCors[rowCt,] <- c(propNames[m],Errs[k],siteIDs[i], boots[j], cor1)

        cat("Chronology Property", propNames[m], "Error Method:",Errs[k],"siteID:",siteIDs[i], "Bootstrapping:", boots[j],"correlation:", cor1, "\n")
#
#
#         cor(allChronProps$value[allChronProps$siteID == siteIDs[i] & allChronProps$chronProp == chronPropNames[m]],
#             allCapture$value[allCapture$IC==0.5 & allCapture$boot == boots[j] & allCapture$Err == Errs[k] & allCapture$siteID == siteIDs[i]])
#
#
#         allCapture$value[
#           allCapture$IC==unique(allCapture$IC)[1] &
#             allCapture$Boot == unique(allCapture$Boot)[1] &
#             allCapture$VE == unique(allCapture$VE)[1] &
#             allCapture$Site == unique(allCapture$Site)[1]]

      }
    }
  }
}

capturePropertyCors$Correlation <- round(as.numeric(capturePropertyCors$Correlation), 2)

capturePropertyCors[order(as.numeric(capturePropertyCors$Correlation)),]

plotDat <- melt(capturePropertyCors)
names(plotDat)
plotDat$Err_Method

pal1 <- c("#cc8b86","#54494b","#508484","#f3a712","#cfd11a")

plotDat$Reconstruction_Error <- factor(plotDat$Err_Method, levels = c("VET0.5", "VEE0.5", "VET0.9","VEE0.9"))#,
                                       #labels = c("50% Theoretical", "50% Empircal", "90% Theoretical", "90% Empircal"))

plotDat$Site1 <- factor(plotDat$Site, levels = c("AM", "RSR", "TN"),
                                       labels = c("AM", "RSR", "TN"))

plotDat$Prop1 <- factor(plotDat$Property, levels = c("AR1Dat", "corDat", "rbarDat", "sdDat"),
                        labels = c("AR1", "Target Cor", "RBAR", "Sample Depth"))

plotDat$Boot1 <- factor(plotDat$Boot, levels = c("None", "ME", "trad"),
                        labels = c("None", "MEboot", "Trad"))

ggplot(data=plotDat, mapping=aes(x=Prop1, y=value, color=Site1, shape=Reconstruction_Error)) +
  geom_hline(yintercept = 0.25, color="red", lty=2)+
  geom_hline(yintercept = -0.25, color="red", lty=2)+
  scale_shape_manual(values = c(15:18))+
  scale_color_manual(values = pal1)+
  theme_classic()+
  scale_y_continuous(limits = c(-0.75, 0.75), breaks = c(-.6,0,.6))+
  #geom_boxplot(aes(x=Property, y=value), inherit.aes = FALSE)+
  geom_jitter(width=0.1, size=2.5)+
  facet_grid(Boot1~Prop1, scales = "free_x")+
  ylab("Correlation to Confidence Interval Capture")+
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

#ggsave(filename = "C:/Users/dce72/Documents/GradSchool/Uncertainty/Fig5New100.png", width = 7, height = 4)






















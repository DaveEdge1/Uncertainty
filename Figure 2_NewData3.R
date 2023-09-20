#clear workspace
rm(list=ls())
gc()

library(ggplot2)
library(reshape2)
library(ggbeeswarm)

newCaptureDat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\NewCICvalues_October22.rds")
runDetails <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\NewCICvalues_October22_rundetails.rds")

#1:1212 is the old chronos
#1213:2424 is the new
#1:12 is real chronos
#13:1212 is simulations
#order: tree Nob, CA, CANA
OLD1 <- c(rep("TN", 4), rep("RSR", 4), rep("AM", 4), rep("TN", 400), rep("RSR", 400), rep("AM", 400))
NEW1 <- c(rep("TN", 4), rep("RSR", 4), rep("AM", 4), rep("TN", 400), rep("RSR", 400), rep("AM", 400))
ALL1 <- c(OLD1, NEW1)

chrono1 <- unlist(lapply(runDetails, function(x) x$Chronology))
boot1 <- unlist(lapply(runDetails, function(x) x$BootstMethod))
IC1 <- unlist(lapply(runDetails, function(x) x$`Prediction Interval`))
CICvetnoboot <- unlist(lapply(newCaptureDat, function(x) mean(x$Captures$Capture1)))
CICveenoboot <- unlist(lapply(newCaptureDat, function(x) mean(x$Captures$Capture2)))
CICvetboot <- unlist(lapply(newCaptureDat, function(x) mean(x$Captures$Capture3)))
CICveeboot <- unlist(lapply(newCaptureDat, function(x) mean(x$Captures$Capture4)))

chronNum <- unlist(lapply(runDetails, function(x) x$ChronNumber))

vetnoboot <- data.frame(IC=IC1,
           Chrono=chrono1,
           Site=ALL1,
           Boot=rep("None", 2424),
           VE=rep("VET", 2424),
           CIC=CICvetnoboot,
           chronNums= chronNum)

veenoboot <- data.frame(IC=IC1,
                        Chrono=chrono1,
                        Site=ALL1,
                        Boot=rep("None", 2424),
                        VE=rep("VEE", 2424),
                        CIC=CICveenoboot,
                        chronNums = chronNum)

vetboot <- data.frame(IC=IC1,
                        Chrono=chrono1,
                        Site=ALL1,
                        Boot=boot1,
                        VE=rep("VET", 2424),
                        CIC=CICvetboot,
                      chronNums = chronNum)

veeboot <- data.frame(IC=IC1,
                        Chrono=chrono1,
                        Site=ALL1,
                        Boot=boot1,
                        VE=rep("VEE", 2424),
                        CIC=CICveeboot,
                      chronNums = chronNum)

rm(newCaptureDat)

allPlotData <- rbind(vetnoboot[c(1213, 1214, 1217, 1218, 1221, 1222,1225:2424),], veenoboot[c(1213, 1214, 1217, 1218, 1221, 1222,1225:2424),],
                     vetboot[13:nrow(vetboot),], veeboot[13:nrow(veeboot),])

allPlotData <- melt(allPlotData, id.vars = c("IC", "Chrono", "Site", "Boot", "VE", "chronNums"))

allPlotData$variable <- paste0(allPlotData$Site, allPlotData$Boot)

#allPlotData$boot1 <- NA
allPlotData$boot1 <- factor(allPlotData$Boot, levels = c("None", "ME", "trad"))

allPlotData$Site[allPlotData$Site == "TN"] <- "Tree Nob"
allPlotData$Site[allPlotData$Site == "RSR"] <- "Rock Springs Ranch"
allPlotData$Site[allPlotData$Site == "AM"] <- "Arrowsmith Mountain"

allPlotData$Chrono[allPlotData$Chrono == "sim1"] <- "Synthetic"
allPlotData$Chrono[allPlotData$Chrono == "chronology"] <- "Original"

allPlotData <- allPlotData[order(allPlotData$Chrono, decreasing = TRUE),]

captureNames <- c(
  'AM' = "Arrowsmith Mountain",
  'RSR' = "Rock Springs Ranch",
  'TN' = "Tree Nob",
  '0.5' = "Fifty, Theoretical"
)

pal1 <- c("#cc8b86","#508484","#f3a712","#cfd11a")

p1 <- ggplot(data=allPlotData[allPlotData$IC == 0.5 & allPlotData$VE == "VET",], mapping = aes(x=variable, y=value, fill=Site)) +
  geom_hline(yintercept = 0.5, lty=2, color=alpha("black", 0.5))+
  geom_violin() +
  geom_quasirandom(alpha = 0.8, width = 0.16, mapping = aes(shape=Chrono, size=Chrono, color=Chrono)) +
  scale_size_manual(values = c(2,.2))+
  scale_color_manual(values = c("black", "grey30"))+
  scale_shape_manual(values = c(17,20))+
  facet_grid(IC~boot1, scales = "free", space="free_x", labeller = as_labeller(captureNames)) +
  #scale_fill_brewer(palette="BrBG") +
  scale_fill_manual(values = pal1, breaks = c("Tree Nob", "Rock Springs Ranch", "Arrowsmith Mountain"))+
  theme_classic()+
  scale_y_continuous(limits = c(.4,.70))+
  #scale_fill_discrete(breaks=c('B', 'C', 'A'))+
  ylab(" ")+
  theme(
    strip.background.x = element_blank(),
    strip.text.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    text = element_text(size = 10, color = "black"),
    legend.title = element_blank()
  )
#p1

captureNames <- c(
  'AM' = "Arrowsmith Mountain",
  'RSR' = "Rock Springs Ranch",
  'TN' = "Tree Nob",
  '0.5' = "Fifty, Empirical"
)

p2 <- ggplot(data=allPlotData[allPlotData$IC == 0.5 & allPlotData$VE == "VEE",], mapping = aes(x=variable, y=value, fill=Site)) +
  geom_hline(yintercept = 0.5, lty=2, color=alpha("black", 0.5))+
  geom_violin() +
  geom_quasirandom(alpha = 0.8, width = 0.16, mapping = aes(shape=Chrono, size=Chrono, color=Chrono)) +
  scale_size_manual(values = c(2,.2))+
  scale_color_manual(values = c("black", "grey30"))+
  scale_shape_manual(values = c(17,20))+
  facet_grid(IC~boot1, scales = "free", space="free_x", labeller = as_labeller(captureNames)) +
  #scale_fill_brewer(palette="BrBG") +
  scale_fill_manual(values = pal1, breaks = c("Tree Nob", "Rock Springs Ranch", "Arrowsmith Mountain"))+
  theme_classic()+
  scale_y_continuous(limits = c(.4,.70))+
  #scale_fill_discrete(breaks=c('B', 'C', 'A'))+
  ylab(" ")+
  theme(
    strip.background.x = element_blank(),
    strip.text.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    text = element_text(size = 10, color = "black"),
    legend.title = element_blank()
  )
#p2

captureNames <- c(
  'AM' = "Arrowsmith Mountain",
  'RSR' = "Rock Springs Ranch",
  'TN' = "Tree Nob",
  '0.9' = "Ninety, Theoretical"
)

p3 <- ggplot(data=allPlotData[allPlotData$IC == 0.9 & allPlotData$VE == "VET",], mapping = aes(x=variable, y=value, fill=Site)) +
  geom_hline(yintercept = 0.9, lty=2, color=alpha("black", 0.5))+
  geom_violin() +
  geom_quasirandom(alpha = 0.8, width = 0.16, mapping = aes(shape=Chrono, size=Chrono, color=Chrono)) +
  scale_size_manual(values = c(2,.2))+
  scale_color_manual(values = c("black", "grey30"))+
  scale_shape_manual(values = c(17,20))+
  facet_grid(IC~boot1, scales = "free", space="free_x", labeller = as_labeller(captureNames)) +
  #scale_fill_brewer(palette="BrBG") +
  scale_fill_manual(values = pal1, breaks = c("Tree Nob", "Rock Springs Ranch", "Arrowsmith Mountain"))+
  theme_classic()+
  scale_y_continuous(limits = c(.8,1))+
  #scale_fill_discrete(breaks=c('B', 'C', 'A'))+
  ylab(" ")+
  theme(
    strip.background.x = element_blank(),
    strip.text.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    text = element_text(size = 10, color = "black"),
    legend.title = element_blank()
  )
#p3
#
captureNames <- c(
  'None' = "None",
  'ME' = "MEboot",
  'trad' = "Traditional",
  '0.9' = "Ninety, Empirical"
)

p4 <- ggplot(data=allPlotData[allPlotData$IC == 0.9 & allPlotData$VE == "VEE",], mapping = aes(x=variable, y=value, fill=Site)) +
  geom_hline(yintercept = 0.9, lty=2, color=alpha("black", 0.5))+
  geom_violin() +
  geom_quasirandom(alpha = 0.8, width = 0.16, mapping = aes(shape=Chrono, size=Chrono, color=Chrono)) +
  scale_size_manual(values = c(2,.2))+
  scale_color_manual(values = c("black", "grey30"))+
  scale_shape_manual(values = c(17,20))+
  facet_grid(IC~boot1, scales = "free", space="free_x", labeller = as_labeller(captureNames)) +
  #scale_fill_brewer(palette="BrBG") +
  scale_fill_manual(values = pal1, breaks = c("Tree Nob", "Rock Springs Ranch", "Arrowsmith Mountain"))+
  theme_classic()+
  scale_y_continuous(limits = c(.8,1))+
  #scale_fill_discrete(breaks=c('B', 'C', 'A'))+
  ylab(" ")+
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    text = element_text(size = 10, color = "black"),
    legend.title = element_blank()
  )

#p4

ggpubr::ggarrange(p4, p3, p2, p1, ncol = 1, common.legend = TRUE, legend = "bottom")

head(allPlotData)
#ggsave(filename = "C:/Users/dce72/Documents/GradSchool/Uncertainty/Fig2NEWOCT23.png", width = 7, height = 7)


#saveRDS(allPlotData, "C:/Users/dce72/Documents/GradSchool/Uncertainty/CaptureDatNewOct25.RData")


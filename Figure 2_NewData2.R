#clear workspace
rm(list=ls())
gc()

#Real chronology
test1RealDat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\TreeNobTest2.RData")
test2RealDat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\ca646Test2.RData")
test4RealDat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\cana113test2.RData")

#simulation
test1Dat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\TreeNobSimTest2.RData")
test2Dat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\ca646SimTest2.RData")
test4Dat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\cana113SimTest2.RData")



plotDataSave <- list()

for (i in 1:3){
  testNow <- list(test1Dat, test2Dat, test4Dat)[[i]]
  testNowReal <- list(test1RealDat, test2RealDat, test4RealDat)[[i]]
  siteID <- c("TN", "RSR", "AM")[i]
  siteIDlong <- c("Tree_Nob", "Rock_Springs_Ranch", "Arrowsmith_Mountain")[i]


  realME50 <- testNowReal[[1]]$CaptureSummary
  realME90 <- testNowReal[[2]]$CaptureSummary
  realtrad50 <- testNowReal[[3]]$CaptureSummary
  realtrad90 <- testNowReal[[4]]$CaptureSummary

  RealTN1 <- mean(c(realME50[1:2], realtrad50[1:2]))
  RealTN2 <- mean(realME50[3:4])
  RealTN3 <- mean(realtrad50[3:4])
  RealTN4 <- mean(c(realME90[1], realtrad90[1]))
  RealTN5 <- mean(c(realME90[2], realtrad90[2]))
  RealTN6 <- realME90[3]
  RealTN7 <- realME90[4]
  RealTN8 <- realtrad90[3]
  RealTN9 <- realtrad90[4]


  ME5 <- seq(1,400,4)
  ME9 <- seq(2,400,4)
  trad5 <- seq(3,400,4)
  trad9 <- seq(4,400,4)

  #lapply(testNow, function(x) x$runDetails)[ME5]

  a1 <- unlist(lapply(testNow, function(x) x$CaptureSummary[1])[ME5])
  a2 <- unlist(lapply(testNow, function(x) x$CaptureSummary[2])[ME5])
  a3 <- unlist(lapply(testNow, function(x) x$CaptureSummary[3])[ME5])
  a4 <- unlist(lapply(testNow, function(x) x$CaptureSummary[4])[ME5])

  b1 <- unlist(lapply(testNow, function(x) x$CaptureSummary[1])[ME9])
  b2 <- unlist(lapply(testNow, function(x) x$CaptureSummary[2])[ME9])
  b3 <- unlist(lapply(testNow, function(x) x$CaptureSummary[3])[ME9])
  b4 <- unlist(lapply(testNow, function(x) x$CaptureSummary[4])[ME9])

  c1 <- unlist(lapply(testNow, function(x) x$CaptureSummary[1])[trad5])
  c2 <- unlist(lapply(testNow, function(x) x$CaptureSummary[2])[trad5])
  c3 <- unlist(lapply(testNow, function(x) x$CaptureSummary[3])[trad5])
  c4 <- unlist(lapply(testNow, function(x) x$CaptureSummary[4])[trad5])

  d1 <- unlist(lapply(testNow, function(x) x$CaptureSummary[1])[trad9])
  d2 <- unlist(lapply(testNow, function(x) x$CaptureSummary[2])[trad9])
  d3 <- unlist(lapply(testNow, function(x) x$CaptureSummary[3])[trad9])
  d4 <- unlist(lapply(testNow, function(x) x$CaptureSummary[4])[trad9])

  TN1 <- colMeans(rbind(a1,a2,c1,c2))
  TN2 <- colMeans(rbind(a3,a4))
  TN3 <- colMeans(rbind(c3,c4))


  library(reshape2)
  library(ggplot2)
  library(ggbeeswarm)

  all50 <- data.frame(TN1,TN2,TN3)
  all50 <- rbind(all50, c(RealTN1,RealTN2, RealTN3))
  all50$chron <- NA
  all50$chron[1:100] <- "Simulation"
  all50$chron[101] <- "Original_Chron"
  plot50 <- melt(all50, id.vars = "chron")
  head(plot50)

  #Set boot IDs
  plot50$boot <- NA
  plot50$boot[as.character(plot50$variable) == "TN1"] <- "None"
  plot50$boot[as.character(plot50$variable) == "TN2"] <- "MEboot"
  plot50$boot[as.character(plot50$variable) == "TN3"] <- "Traditional"
  #Set error method IDs
  plot50$Err <- "Emp/Theor"

  plot50$capture <- "Fifty"


  ggplot(data=plot50, mapping = aes(x=variable, y=value, fill=boot, color=Err)) + geom_violin() +
    geom_quasirandom(alpha = 0.8, width = 0.3, mapping = aes(shape=chron, size=chron), color="black") +
    scale_fill_brewer(palette="BrBG") + theme_classic()+
    scale_size_manual(values = c(2,1))

  TN4 <- colMeans(rbind(b1,d1))
  TN5 <- colMeans(rbind(b2,d2))
  TN6 <- b3
  TN7 <- b4
  TN8 <- d3
  TN9 <- d4

  all90 <- data.frame(TN4,TN5,TN6, TN7, TN8, TN9)
  all90 <- rbind(all90, c(RealTN4,RealTN5, RealTN6,RealTN7,RealTN8, RealTN9))
  all90$chron <- NA
  all90$chron[1:100] <- "Simulation"
  all90$chron[101] <- "Original_Chron"
  plot90 <- melt(all90, id.vars = "chron")
  head(plot90)

  #Set boot and error method IDs
  plot90$boot <- NA
  plot90$Err <- NA
  #plot90$Err[as.character(plot90$variable) %in% c("TN4","TN5")] <- "emp/theor"
  plot90$boot[as.character(plot90$variable) %in% c("TN6","TN7")] <- "MEboot"
  plot90$boot[as.character(plot90$variable) %in% c("TN8","TN9")] <- "Traditional"
  plot90$boot[as.character(plot90$variable) %in% c("TN4","TN5")] <- "None"
  plot90$Err[as.character(plot90$variable) %in% c("TN4","TN6","TN8")] <- "Theoretical"
  plot90$Err[as.character(plot90$variable) %in% c("TN5","TN7","TN9")] <- "Empirical"

  plot90$capture <- "Ninety"

  ggplot(data=plot90, mapping = aes(x=variable, y=value, fill=boot, color=Err)) + geom_violin() +
    geom_quasirandom(alpha = 0.8, width = 0.2, mapping = aes(shape=chron, size=chron), color="black") +
    scale_fill_brewer(palette="BrBG") + theme_classic()+
    scale_size_manual(values = c(2,1))+
    scale_shape_manual(values = c(17,20))


  plotAll <- rbind.data.frame(plot50, plot90)

  plotAll$variable <- paste0(siteID, substring(plotAll$variable, first = 3, last=3))
  plotAll$variable <- as.factor(plotAll$variable)

  plotAll$site <- siteIDlong
  colnames(plotAll)[3] <- "Percent_Capture"

  plotDataSave[[i]] <- plotAll
}
head(plotAll)

plotAll3Sites <- rbind.data.frame(plotDataSave[[1]], plotDataSave[[2]], plotDataSave[[3]])

# p1 <- ggplot(data=plotAll3Sites[plotAll3Sites$capture == "Fifty",], mapping = aes(x=variable, y=Percent_Capture, fill=boot, shape=Err), lwd=5) + geom_violin() +
#   geom_jitter(position=position_jitter(0.2)) +
#   facet_grid(capture~boot, scales = "free", space="free_x") +
#   scale_fill_brewer(palette="BrBG") + theme_classic()+
#   theme(
#     strip.background.x = element_blank(),
#     strip.text.x = element_blank(),
#     axis.text.x = element_blank(),
#     axis.title.x = element_blank()
#   )
#
# p2 <- ggplot(data=plotAll3Sites[plotAll3Sites$capture == "Ninety",], mapping = aes(x=variable, y=Percent_Capture, fill=boot, shape=Err), lwd=5) + geom_violin() +
#   geom_jitter(position=position_jitter(0.2)) +
#   facet_grid(capture~boot, scales = "free", space="free_x") +
#   scale_fill_brewer(palette="BrBG") + theme_classic() +
#   theme(
#     axis.text.x = element_blank(),
#     axis.title.x = element_blank()
#   )
#
# ggpubr::ggarrange(p2, p1, ncol = 1, common.legend = TRUE, legend = "bottom")


# ggplot(data=plot90, mapping = aes(x=variable, y=value, fill=boot, color=Err)) + geom_violin() +
#   geom_quasirandom(alpha = 0.8, width = 0.2, mapping = aes(shape=chron, size=chron), color="black") +
#   scale_fill_brewer(palette="BrBG") + theme_classic()+
#   scale_size_manual(values = c(2,1))+
#   scale_shape_manual(values = c(17,20))

lowFidelitySimsFig2 <- readRDS(file = "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\lowFidelitySimsFig2.RData")

lowFidelitySimsFig2 <- lowFidelitySimsFig2[lowFidelitySimsFig2$chron != "Original_Chron",]

plotAll3Sites <- rbind(plotAll3Sites, lowFidelitySimsFig2)


p1 <- ggplot(data=plotAll3Sites[plotAll3Sites$capture == "Fifty",], mapping = aes(x=variable, y=Percent_Capture, fill=boot)) +
  geom_hline(yintercept = 0.5, lty=2, color=alpha("black", 0.5))+
  geom_violin() +
  geom_quasirandom(alpha = 0.8, width = 0.16, mapping = aes(shape=chron, size=chron, color=chron)) +
  scale_size_manual(values = c(2,.2))+
  scale_color_manual(values = c("black", "grey30"))+
  scale_shape_manual(values = c(17,20))+
  facet_grid(capture~site, scales = "free", space="free_x") +
  scale_fill_brewer(palette="BrBG") + theme_classic()+
  scale_y_continuous(limits = c(.45,.70))+
  ylab(" ")+
  theme(
    strip.background.x = element_blank(),
    strip.text.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    text = element_text(size = 10, color = "black"),
    legend.title = element_blank()
  )


captureNames <- c(
  'Ninety' = "Ninety, Theoretical",
  'Fifty' = "Fifty, Theoretical",
  'Tree_Nob' = "Tree Nob",
  'Rock_Springs_Ranch' = "Rock Springs Ranch",
  'Arrowsmith_Mountain' = "Arrowsmith Mountain"
)

p2 <- ggplot(data=plotAll3Sites[plotAll3Sites$capture == "Ninety" & plotAll3Sites$Err == "Theoretical",], mapping = aes(x=variable, y=Percent_Capture, fill=boot)) +
  geom_hline(yintercept = 0.9, lty=2, color=alpha("black", 0.5))+  geom_violin() +
  geom_quasirandom(alpha = 0.8, width = 0.16, mapping = aes(shape=chron, size=chron, color=chron)) +
  scale_size_manual(values = c(2,.2))+
  scale_color_manual(values = c("black", "grey30"))+
  scale_shape_manual(values = c(17,20))+
  facet_grid(capture~site, scales = "free", space="free_x", labeller = as_labeller(captureNames)) +
  scale_fill_brewer(palette="BrBG") + theme_classic() +
  scale_y_continuous(limits = c(.75,1))+
  ylab("Percent Capture")+
  theme(
    strip.background.x = element_blank(),
    strip.text.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    text = element_text(size = 10, color = "black"),
    legend.title = element_blank()
  )

captureNames <- c(
  'Ninety' = "Ninety, Empirical",
  'Fifty' = "Fifty, Empirical",
  'Tree_Nob' = "Tree Nob",
  'Rock_Springs_Ranch' = "Rock Springs Ranch",
  'Arrowsmith_Mountain' = "Arrowsmith Mountain"
)


p3 <- ggplot(data=plotAll3Sites[plotAll3Sites$capture == "Ninety" & plotAll3Sites$Err == "Empirical",], mapping = aes(x=variable, y=Percent_Capture, fill=boot)) +
  geom_hline(yintercept = 0.9, lty=2, color=alpha("black", 0.5))+
  geom_violin() +
  geom_quasirandom(alpha = 0.8, width = 0.16, mapping = aes(shape=chron, size=chron, color=chron)) +
  scale_color_manual(values = c("black", "grey30"))+
  scale_size_manual(values = c(2,.2))+
  scale_shape_manual(values = c(17,20))+
  facet_grid(capture~site, scales = "free", space="free_x", labeller = as_labeller(captureNames)) +
  scale_fill_brewer(palette="BrBG") + theme_classic() +
  scale_y_continuous(limits = c(.75,1))+
  ylab(" ")+
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    text = element_text(size = 10, color = "black"),
    legend.title = element_blank()
  )

ggpubr::ggarrange(p3, p2, p1, ncol = 1, common.legend = TRUE, legend = "bottom", heights = c(1,.9,.9))


ggsave(filename = "C:/Users/dce72/Documents/GradSchool/Uncertainty/Fig2.png", width = 7, height = 7)










#clear workspace
rm(list=ls())
gc()

#test1Dat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\TreeNobSimTest.RData")

test1Dat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test1.RData")
test2Dat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test2.RData")
test4Dat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test4.RData")

plotDataSave <- list()

for (i in 1:3){
  testNow <- list(test1Dat, test2Dat, test4Dat)[[i]]
  siteID <- c("TN", "RSR", "AM")[i]
  siteIDlong <- c("Tree_Nob", "Rock_Springs_Ranch", "Arrowsmith_Mountain")[i]

  ME5 <- seq(5,41,4)
  ME9 <- seq(6,42,4)
  trad5 <- seq(7,43,4)
  trad9 <- seq(8,44,4)

  # ME5 <- seq(1,400,4)
  # ME9 <- seq(2,400,4)
  # trad5 <- seq(3,400,4)
  # trad9 <- seq(4,400,4)

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

  all50 <- data.frame(TN1,TN2,TN3)
  plot50 <- melt(all50)
  head(plot50)

  #Set boot IDs
  plot50$boot <- NA
  plot50$boot[as.character(plot50$variable) == "TN1"] <- "None"
  plot50$boot[as.character(plot50$variable) == "TN2"] <- "MEboot"
  plot50$boot[as.character(plot50$variable) == "TN3"] <- "Traditional"
  #Set error method IDs
  plot50$Err <- "Emp/Theor"

  plot50$capture <- "Fifty"


  #ggplot(data=plot50, mapping = aes(x=variable, y=value, fill=boot, =Err)) + geom_violin()

  TN4 <- colMeans(rbind(b1,d1))
  TN5 <- colMeans(rbind(b2,d2))
  TN6 <- b3
  TN7 <- b4
  TN8 <- d3
  TN9 <- d4

  all90 <- data.frame(TN4, TN5, TN6, TN7, TN8, TN9)
  plot90 <- melt(all90)
  head(plot90)

  #Set boot and error method IDs
  plot90$boot <- NA
  plot90$Err <- NA
  #plot90$Err[as.character(plot90$variable) %in% c("TN4","TN5")] <- "emp/theor"
  plot90$boot[as.character(plot90$variable) %in% c("TN6","TN7")] <- "MEboot"
  plot90$boot[as.character(plot90$variable) %in% c("TN8","TN9")] <- "Traditional"
  plot90$boot[as.character(plot90$variable) %in% c("TN4","TN5")] <- "None"
  plot90$Err[as.character(plot90$variable) %in% c("TN5","TN6","TN8")] <- "Theoretical"
  plot90$Err[as.character(plot90$variable) %in% c("TN4","TN7","TN9")] <- "Empirical"

  plot90$capture <- "Ninety"

  #ggplot(data=plot90, mapping = aes(x=variable, y=value, fill=boot, color=Err), lwd=5) + geom_violin()

  plotAll <- rbind.data.frame(plot50, plot90)

  plotAll$variable <- paste0(siteID, substring(plotAll$variable, first = 3, last=3))
  plotAll$variable <- as.factor(plotAll$variable)

  plotAll$site <- siteIDlong
  colnames(plotAll)[2] <- "Percent_Capture"

  plotDataSave[[i]] <- plotAll

  if(i > 1){
    plotAll <- rbind.data.frame(plotAll, plotAll)
  }


}

plotAll3Sites <- rbind.data.frame(plotDataSave[[1]], plotDataSave[[2]], plotDataSave[[3]])

p1 <- ggplot(data=plotAll3Sites[plotAll3Sites$capture == "Fifty",], mapping = aes(x=variable, y=Percent_Capture, fill=boot, shape=Err), lwd=5) + geom_violin() +
  geom_jitter(position=position_jitter(0.2)) +
  facet_grid(capture~boot, scales = "free", space="free_x") +
  scale_fill_brewer(palette="BrBG") + theme_classic()+
  theme(
    strip.background.x = element_blank(),
    strip.text.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )

p2 <- ggplot(data=plotAll3Sites[plotAll3Sites$capture == "Ninety",], mapping = aes(x=variable, y=Percent_Capture, fill=boot, shape=Err), lwd=5) + geom_violin() +
  geom_jitter(position=position_jitter(0.2)) +
  facet_grid(capture~boot, scales = "free", space="free_x") +
  scale_fill_brewer(palette="BrBG") + theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )

ggpubr::ggarrange(p2, p1, ncol = 1, common.legend = TRUE, legend = "bottom")



p1 <- ggplot(data=plotAll3Sites[plotAll3Sites$capture == "Fifty",], mapping = aes(x=variable, y=Percent_Capture, fill=boot, shape=Err), lwd=5) + geom_violin() +
  geom_jitter(position=position_jitter(0.2)) +
  facet_grid(capture~site, scales = "free", space="free_x") +
  scale_fill_brewer(palette="BrBG") + theme_classic()+
  theme(
    strip.background.x = element_blank(),
    strip.text.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )

p2 <- ggplot(data=plotAll3Sites[plotAll3Sites$capture == "Ninety",], mapping = aes(x=variable, y=Percent_Capture, fill=boot, shape=Err), lwd=5) + geom_violin() +
  geom_jitter(position=position_jitter(0.2)) +
  facet_grid(capture~site, scales = "free", space="free_x") +
  scale_fill_brewer(palette="BrBG") + theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )

ggpubr::ggarrange(p2, p1, ncol = 1, common.legend = TRUE, legend = "bottom")






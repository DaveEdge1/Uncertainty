#clear workspace
rm(list=ls())
gc()

library(dplR)
library(treeclim)

cana113 <- read.rwl("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\SimpleBootTrials\\cana113.rwl")
temp <- read.csv("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\SimpleBootTrials\\cana113temp.csv")

#cana113rwi <- dplR::detrend(cana113, method = "Spline")
cana113chron <- chron(cana113)
#cana113chron1 <- chron(cana113)
#cor(cana113chron$std, cana113chron1$std)

#detrend.series(cana113$`694011`, method = "Spline")
#rwi.stats(cana113rwi)
rwi.stats(cana113)

df2 <- dcc(
  cana113chron,
  temp,
  selection = -10:12,
  method = "correlation",
  dynamic = "static",
  win_size = 25,
  win_offset = 1,
  start_last = TRUE,
  timespan = NULL,
  var_names = NULL,
  ci = 0.05,
  boot = "stationary",
  sb = TRUE
)

plot(df2)

df2 <- dcc(
  cana113chron,
  temp,
  selection = .mean(4:8)+.mean(4:10)+.mean(3:8)+.mean(3:10),
  method = "correlation",
  dynamic = "static",
  win_size = 25,
  win_offset = 1,
  start_last = TRUE,
  timespan = NULL,
  var_names = NULL,
  ci = 0.05,
  boot = "stationary",
  sb = TRUE
)


plot(df2)

head(temp)

AprOctAvgTemp <- apply(temp[,5:11], 1, function(x) mean(x))
climYear <- temp[,1]


canaTempTarget <- data.frame("Year" = climYear,
                                "AprOctAvgTemp" = AprOctAvgTemp)

cor(cana113chron$std[as.numeric(rownames(cana113)) %in% canaTempTarget$Year], 
    canaTempTarget$AprOctAvgTemp[canaTempTarget$Year %in% as.numeric(rownames(cana113))])

write.csv(canaTempTarget, "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\SimpleBootTrials\\canaTempTarget.csv")
#write.csv(cana113rwi, "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\SimpleBootTrials\\cana113rwi.csv")  

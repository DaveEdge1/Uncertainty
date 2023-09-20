#clear workspace
rm(list=ls())
gc()

library(dplR)
library(treeclim)

ca646 <- read.rwl("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\SimpleBootTrials\\ca646.rwl")
precip <- read.csv("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\SimpleBootTrials\\ca646precip.csv")
pdsi <- read.csv("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\SimpleBootTrials\\Ca646PDSI.csv")

ca646rwi <- dplR::detrend(ca646, method = "Spline")
ca646chron <- chron(ca646rwi)

df2 <- dcc(
  ca646chron,
  precip,
  selection = .mean(-12:2)+ .mean(-6:-8),
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
df2$truncated$climate

plot(df2)

JFprecipSum <- c(
  precip$Jan[precip$X %in% as.numeric(rownames(ca646chron))]+
    precip$Feb[precip$X %in% as.numeric(rownames(ca646chron))])

cor(ca646chron$std[as.numeric(rownames(ca646chron)) %in% precip$X], precip$Jan[precip$X %in% as.numeric(rownames(ca646chron))])
cor(ca646chron$std[as.numeric(rownames(ca646chron)) %in% precip$X], JFprecipSum)

ca646PrecipTarget <- data.frame("Year" = precip$X[precip$X %in% as.numeric(rownames(ca646chron))],
                                "JFprecipSum" = JFprecipSum)

# write.csv(ca646PrecipTarget, "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\SimpleBootTrials\\ca646PrecipTarget.csv")
# write.csv(ca646rwi, "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\SimpleBootTrials\\ca646rwi.csv")  

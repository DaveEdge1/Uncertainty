#clear workspace
rm(list=ls())
gc()
#load package(s)
require(dplR)
require(forecast)




#load Tree Nob detrended ring width data
climDat1 <- read.csv("C:/Users/dce72/Documents/GradSchool/Uncertainty/SimpleBootTrials/LangaraSST.csv")
climDat1 <- climDat1[climDat1$Year < 2002,]
TNchron <- read.rwl("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\SimpleBootTrials\\TreeNobRCStrunc1.csv")
TNchron <- TNchron[as.numeric(rownames(TNchron))>1725,]
TNchron <- TNchron[,apply(TNchron, 2, function(x) sum(!is.na(x)))>0]
TNmvchron <- chron(TNchron)
cor(TNmvchron[as.numeric(rownames(TNmvchron)) %in% climDat1[,1],1], climDat1[climDat1[,1] %in% as.numeric(rownames(TNmvchron)),2])
rwi.stats(TNchron)


#######################################################################################################################
########################
#Tree Nob and
#TN Sim1
########################
#######################################################################################################################

########################
#Summary Results Table
########################

TNchron <- read.rwl("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\SimpleBootTrials\\TreeNobRCStrunc1.csv")
TNchron <- TNchron[as.numeric(rownames(TNchron))>1725,]
test1Dat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test1.RData")

lapply(test1Dat, function(x) x$runDetails$Chronology)


########################
#Gather rwi stats
########################

TnSynRWI <- list()
for (i in 1:40){
  k=i+4
  TnSynRWI[[i]] <- rwi.stats(test1Dat[[k]]$synchron$synChron)
}

rwi.stats(TNchron)
TnSynRWI[seq(1,40,by=4)]
for(i in 1:13){
  cat(names(TnSynRWI[[1]])[i], mean(unlist(lapply(TnSynRWI, function(x) unlist(x)[[i]]))), "\n")
}
########################
#Get AR1 values
########################
test1Dat[[5]]$synchron$synChronData$origChronAR1
mean(unlist(lapply(test1Dat[5:44], function(x) x$synchron$synChronData$surChronAR1)))

########################
#Get correlations to target
########################
cor(TNmvchron[as.numeric(rownames(TNmvchron)) %in% climDat1[,1],1], climDat1[climDat1[,1] %in% as.numeric(rownames(TNmvchron)),2])
mean(unlist(lapply(test1Dat[5:44], function(x) x$synchron$synChronData$surChronTargetCor)))

########################
#Correlations amoung synthetic and orginal chronology
########################
simIndex <- seq(5,41,by=4)
mvSynchrons <- data.frame(matrix(data=NA, ncol = 10, nrow = length(TNmvchron[,1])))
for(i in 1:10){
  mvSynchrons[,i] <- chron(test1Dat[[simIndex[i]]]$synchron$synChron)[,1]
  cat(cor(TNmvchron[,1], mvSynchrons[,i]), "\n")
}
AllTnChrons <- cbind(TNmvchron[,1], mvSynchrons)
names(AllTnChrons)[1] <- c("TreeNob")
for(i in 1:10){
  names(AllTnChrons)[(i+1)] <- paste0("Sim",i)
}

cor(AllTnChrons, AllTnChrons)

########################
#Prediction Interval Testing
########################
lapply(test1Dat, function(x) x$runDetails)

cT1 <- test1Dat[[1]]$CaptureSummary
cT2 <- test1Dat[[2]]$CaptureSummary
cT3 <- test1Dat[[3]]$CaptureSummary
cT4 <- test1Dat[[4]]$CaptureSummary

#lapply(test1Dat, function(x) x$runDetails)[seq(24,44,2)]
ME5 <- seq(5,41,4)
ME9 <- seq(6,42,4)
trad5 <- seq(7,43,4)
trad9 <- seq(8,44,4)

a1 <- mean(unlist(lapply(test1Dat, function(x) x$CaptureSummary[1])[ME5]))
a2 <- mean(unlist(lapply(test1Dat, function(x) x$CaptureSummary[2])[ME5]))
a3 <- mean(unlist(lapply(test1Dat, function(x) x$CaptureSummary[3])[ME5]))
a4 <- mean(unlist(lapply(test1Dat, function(x) x$CaptureSummary[4])[ME5]))

b1 <- mean(unlist(lapply(test1Dat, function(x) x$CaptureSummary[1])[ME9]))
b2 <- mean(unlist(lapply(test1Dat, function(x) x$CaptureSummary[2])[ME9]))
b3 <- mean(unlist(lapply(test1Dat, function(x) x$CaptureSummary[3])[ME9]))
b4 <- mean(unlist(lapply(test1Dat, function(x) x$CaptureSummary[4])[ME9]))

c1 <- mean(unlist(lapply(test1Dat, function(x) x$CaptureSummary[1])[trad5]))
c2 <- mean(unlist(lapply(test1Dat, function(x) x$CaptureSummary[2])[trad5]))
c3 <- mean(unlist(lapply(test1Dat, function(x) x$CaptureSummary[3])[trad5]))
c4 <- mean(unlist(lapply(test1Dat, function(x) x$CaptureSummary[4])[trad5]))

d1 <- mean(unlist(lapply(test1Dat, function(x) x$CaptureSummary[1])[trad9]))
d2 <- mean(unlist(lapply(test1Dat, function(x) x$CaptureSummary[2])[trad9]))
d3 <- mean(unlist(lapply(test1Dat, function(x) x$CaptureSummary[3])[trad9]))
d4 <- mean(unlist(lapply(test1Dat, function(x) x$CaptureSummary[4])[trad9]))

summaryDat1 <- data.frame(row.names = c("ME50", "ME90", "trad50", "trad90"),
                          "ErrTheory" <- c(a1,b1,c1,d1),
                          "ErrEmp" <- c(a2,b2,c2,d2),
                          "ChronoConfEmp" <- c(a3,b3,c3,d3),
                          "ChronoConfTheory" <- c(a4,b4,c4,d4))

summaryDat1 <- rbind.data.frame(summaryDat1,cT1,cT2,cT3,cT4)
colnames(summaryDat1) <- c("ErrTheory","ErrEmp","ChronoConfTheory", "ChronoConfEmp")
rownames(summaryDat1) <- c("simME50", "simME90", "simTrad50", "simTrad90","realME50", "realME90", "realTrad50", "realTrad90")
View(summaryDat1)

#######################################################################################################################
########################
#TN Sim2
########################
#######################################################################################################################

test10Dat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test10.RData")

lapply(test10Dat, function(x) x$runDetails$Chronology)

########################
#Gather rwi stats
########################
TnSyn2RWI <- list()
for (i in 1:40){
  TnSyn2RWI[[i]] <- rwi.stats(test10Dat[[i]]$synchron$synChron)
}

for(i in 1:13){
  cat(names(TnSyn2RWI[[1]])[i], mean(unlist(lapply(TnSyn2RWI, function(x) unlist(x)[[i]]))), "\n")
}

########################
#Get AR1 values
########################
mean(unlist(lapply(test10Dat[1:40], function(x) x$synchron$synChronData$surChronAR1)))

########################
#Get correlations to target
########################
cor(TNmvchron[as.numeric(rownames(TNmvchron)) %in% climDat1[,1],1], climDat1[climDat1[,1] %in% as.numeric(rownames(TNmvchron)),2])
mean(unlist(lapply(test10Dat[1:40], function(x) x$synchron$synChronData$surChronTargetCor)))

########################
#Prediction Interval Testing
########################
lapply(test1Dat, function(x) x$runDetails)

rm(cT1)
rm(cT2)
rm(cT3)
rm(cT4)

lapply(test10Dat, function(x) x$runDetails)[ME5]
ME5 <- seq(1,37,4)
ME9 <- seq(2,38,4)
trad5 <- seq(3,39,4)
trad9 <- seq(4,40,4)

a1 <- mean(unlist(lapply(test10Dat, function(x) x$CaptureSummary[1])[ME5]))
a2 <- mean(unlist(lapply(test10Dat, function(x) x$CaptureSummary[2])[ME5]))
a3 <- mean(unlist(lapply(test10Dat, function(x) x$CaptureSummary[3])[ME5]))
a4 <- mean(unlist(lapply(test10Dat, function(x) x$CaptureSummary[4])[ME5]))

b1 <- mean(unlist(lapply(test10Dat, function(x) x$CaptureSummary[1])[ME9]))
b2 <- mean(unlist(lapply(test10Dat, function(x) x$CaptureSummary[2])[ME9]))
b3 <- mean(unlist(lapply(test10Dat, function(x) x$CaptureSummary[3])[ME9]))
b4 <- mean(unlist(lapply(test10Dat, function(x) x$CaptureSummary[4])[ME9]))

c1 <- mean(unlist(lapply(test10Dat, function(x) x$CaptureSummary[1])[trad5]))
c2 <- mean(unlist(lapply(test10Dat, function(x) x$CaptureSummary[2])[trad5]))
c3 <- mean(unlist(lapply(test10Dat, function(x) x$CaptureSummary[3])[trad5]))
c4 <- mean(unlist(lapply(test10Dat, function(x) x$CaptureSummary[4])[trad5]))

d1 <- mean(unlist(lapply(test10Dat, function(x) x$CaptureSummary[1])[trad9]))
d2 <- mean(unlist(lapply(test10Dat, function(x) x$CaptureSummary[2])[trad9]))
d3 <- mean(unlist(lapply(test10Dat, function(x) x$CaptureSummary[3])[trad9]))
d4 <- mean(unlist(lapply(test10Dat, function(x) x$CaptureSummary[4])[trad9]))

summaryDat2 <- data.frame(row.names = c("ME50", "ME90", "trad50", "trad90"),
                          "ErrTheory" <- c(a1,b1,c1,d1),
                          "ErrEmp" <- c(a2,b2,c2,d2),
                          "ChronoConfEmp" <- c(a3,b3,c3,d3),
                          "ChronoConfTheory" <- c(a4,b4,c4,d4))

colnames(summaryDat2) <- c("ErrTheory","ErrEmp","ChronoConfTheory", "ChronoConfEmp")

summaryDat2

#######################################################################################################################
########################
#TN Sim3
########################
#######################################################################################################################

test11Dat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test11.RData")

lapply(test11Dat, function(x) x$runDetails)


########################
#Gather rwi stats
########################
TnSyn3RWI <- list()
for (i in 1:40){
  TnSyn3RWI[[i]] <- rwi.stats(test11Dat[[i]]$synchron$synChron)
}

for(i in 1:13){
  cat(names(TnSyn3RWI[[1]])[i], mean(unlist(lapply(TnSyn3RWI, function(x) unlist(x)[[i]]))), "\n")
}

########################
#Get AR1 values
########################
mean(unlist(lapply(test11Dat[1:40], function(x) x$synchron$synChronData$surChronAR1)))

########################
#Get correlations to target
########################
cor(TNmvchron[as.numeric(rownames(TNmvchron)) %in% climDat1[,1],1], climDat1[climDat1[,1] %in% as.numeric(rownames(TNmvchron)),2])
mean(unlist(lapply(test11Dat[1:40], function(x) x$synchron$synChronData$surChronTargetCor)))

########################
#Prediction Interval Testing
########################

lapply(test11Dat, function(x) x$runDetails)[ME5]
ME5 <- seq(1,37,4)
ME9 <- seq(2,38,4)
trad5 <- seq(3,39,4)
trad9 <- seq(4,40,4)

a1 <- mean(unlist(lapply(test11Dat, function(x) x$CaptureSummary[1])[ME5]))
a2 <- mean(unlist(lapply(test11Dat, function(x) x$CaptureSummary[2])[ME5]))
a3 <- mean(unlist(lapply(test11Dat, function(x) x$CaptureSummary[3])[ME5]))
a4 <- mean(unlist(lapply(test11Dat, function(x) x$CaptureSummary[4])[ME5]))

b1 <- mean(unlist(lapply(test11Dat, function(x) x$CaptureSummary[1])[ME9]))
b2 <- mean(unlist(lapply(test11Dat, function(x) x$CaptureSummary[2])[ME9]))
b3 <- mean(unlist(lapply(test11Dat, function(x) x$CaptureSummary[3])[ME9]))
b4 <- mean(unlist(lapply(test11Dat, function(x) x$CaptureSummary[4])[ME9]))

c1 <- mean(unlist(lapply(test11Dat, function(x) x$CaptureSummary[1])[trad5]))
c2 <- mean(unlist(lapply(test11Dat, function(x) x$CaptureSummary[2])[trad5]))
c3 <- mean(unlist(lapply(test11Dat, function(x) x$CaptureSummary[3])[trad5]))
c4 <- mean(unlist(lapply(test11Dat, function(x) x$CaptureSummary[4])[trad5]))

d1 <- mean(unlist(lapply(test11Dat, function(x) x$CaptureSummary[1])[trad9]))
d2 <- mean(unlist(lapply(test11Dat, function(x) x$CaptureSummary[2])[trad9]))
d3 <- mean(unlist(lapply(test11Dat, function(x) x$CaptureSummary[3])[trad9]))
d4 <- mean(unlist(lapply(test11Dat, function(x) x$CaptureSummary[4])[trad9]))

summaryDat3 <- data.frame(row.names = c("ME50", "ME90", "trad50", "trad90"),
                          "ErrTheory" <- c(a1,b1,c1,d1),
                          "ErrEmp" <- c(a2,b2,c2,d2),
                          "ChronoConfEmp" <- c(a3,b3,c3,d3),
                          "ChronoConfTheory" <- c(a4,b4,c4,d4))

colnames(summaryDat3) <- c("ErrTheory","ErrEmp","ChronoConfTheory", "ChronoConfEmp")

summaryDat3

#######################################################################################################################
########################
#TN Sim4
########################
#######################################################################################################################

test12Dat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test12.RData")

lapply(test12Dat, function(x) x$runDetails)


########################
#Gather rwi stats
########################
TnSyn4RWI <- list()
for (i in 1:40){
  TnSyn4RWI[[i]] <- rwi.stats(test12Dat[[i]]$synchron$synChron)
}

for(i in 1:13){
  cat(names(TnSyn4RWI[[1]])[i], mean(unlist(lapply(TnSyn4RWI, function(x) unlist(x)[[i]]))), "\n")
}

########################
#Get AR1 values
########################
mean(unlist(lapply(test12Dat[1:40], function(x) x$synchron$synChronData$surChronAR1)))

########################
#Get correlations to target
########################
cor(TNmvchron[as.numeric(rownames(TNmvchron)) %in% climDat1[,1],1], climDat1[climDat1[,1] %in% as.numeric(rownames(TNmvchron)),2])
mean(unlist(lapply(test12Dat[1:40], function(x) x$synchron$synChronData$surChronTargetCor)))

allCors <- rep(NA, 40)
for(i in 1:40){
  test12Dat[[i]]$synchron$synChronData
  synchron1 <- chron(test12Dat[[i]]$synchron$synChron)
  allCors[i] <- cor(synchron1[as.numeric(rownames(synchron1)) %in% climDat1[,1],1], climDat1[climDat1[,1] %in% as.numeric(rownames(synchron1)),2])
}
mean(allCors)

########################
#Prediction Interval Testing
########################

lapply(test12Dat, function(x) x$runDetails)[ME5]
ME5 <- seq(1,37,4)
ME9 <- seq(2,38,4)
trad5 <- seq(3,39,4)
trad9 <- seq(4,40,4)

a1 <- mean(unlist(lapply(test12Dat, function(x) x$CaptureSummary[1])[ME5]))
a2 <- mean(unlist(lapply(test12Dat, function(x) x$CaptureSummary[2])[ME5]))
a3 <- mean(unlist(lapply(test12Dat, function(x) x$CaptureSummary[3])[ME5]))
a4 <- mean(unlist(lapply(test12Dat, function(x) x$CaptureSummary[4])[ME5]))

b1 <- mean(unlist(lapply(test12Dat, function(x) x$CaptureSummary[1])[ME9]))
b2 <- mean(unlist(lapply(test12Dat, function(x) x$CaptureSummary[2])[ME9]))
b3 <- mean(unlist(lapply(test12Dat, function(x) x$CaptureSummary[3])[ME9]))
b4 <- mean(unlist(lapply(test12Dat, function(x) x$CaptureSummary[4])[ME9]))

c1 <- mean(unlist(lapply(test12Dat, function(x) x$CaptureSummary[1])[trad5]))
c2 <- mean(unlist(lapply(test12Dat, function(x) x$CaptureSummary[2])[trad5]))
c3 <- mean(unlist(lapply(test12Dat, function(x) x$CaptureSummary[3])[trad5]))
c4 <- mean(unlist(lapply(test12Dat, function(x) x$CaptureSummary[4])[trad5]))

d1 <- mean(unlist(lapply(test12Dat, function(x) x$CaptureSummary[1])[trad9]))
d2 <- mean(unlist(lapply(test12Dat, function(x) x$CaptureSummary[2])[trad9]))
d3 <- mean(unlist(lapply(test12Dat, function(x) x$CaptureSummary[3])[trad9]))
d4 <- mean(unlist(lapply(test12Dat, function(x) x$CaptureSummary[4])[trad9]))

summaryDat4 <- data.frame(row.names = c("ME50", "ME90", "trad50", "trad90"),
                          "ErrTheory" <- c(a1,b1,c1,d1),
                          "ErrEmp" <- c(a2,b2,c2,d2),
                          "ChronoConfEmp" <- c(a3,b3,c3,d3),
                          "ChronoConfTheory" <- c(a4,b4,c4,d4))

colnames(summaryDat4) <- c("ErrTheory","ErrEmp","ChronoConfTheory", "ChronoConfEmp")

summaryDat4


#######################################################################################################################
#######################################################################################################################
#######################################################################################################################

#ca646 Blue Oak


#load ca646 Blue Oak detrended ring width data
climDat2 <- read.csv("C:/Users/dce72/Documents/GradSchool/Uncertainty/SimpleBootTrials/ca646PrecipTarget.csv")
climDat2 <- climDat2[,-1]
names(climDat2) <- c("Year", "Target")
chron646 <- read.rwl("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\SimpleBootTrials\\ca646rwi.csv")
chron646mv <- chron(chron646)
cor(chron646mv[as.numeric(rownames(chron646mv)) %in% climDat2[,1],1], climDat2[climDat2[,1] %in% as.numeric(rownames(chron646mv)),2])

#gather tree and core ids for rwi stats
ca646IDs <- data.frame(matrix(data=NA, ncol = 2, nrow = ncol(chron646)))
for(i in 1:ncol(chron646)){
  ca646IDs[i,1] <- substr(colnames(chron646)[i],start = 1, stop = 5)
  ca646IDs[i,2] <- substr(colnames(chron646)[i],start = 6, stop = 6)
}
uniqueTree <- unique(ca646IDs[,1])
for(i in 1:length(uniqueTree)){
  ca646IDs[ca646IDs[,1] %in% uniqueTree[i],1] <- i
}
uniqueCore <- unique(ca646IDs[,2])
for(i in 1:length(uniqueCore)){
  ca646IDs[ca646IDs[,2] %in% uniqueCore[i],2] <- i
}
ca646IDs <- apply(ca646IDs, 2, function(x) as.numeric(x))
colnames(ca646IDs) <- c("tree", "core")
ca646IDs <- as.data.frame(ca646IDs)
rwi.stats(chron646, ids = ca646IDs)

#######################################################################################################################
########################
#ca646 Blue Oak
#ca646 Sim1
########################
#######################################################################################################################

########################
#Summary Results Table
########################

test2Dat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test2.RData")

lapply(test2Dat, function(x) x$runDetails$Chronology)


########################
#Gather rwi stats
########################

Ca646SynRWI <- list()
for (i in 1:40){
  k=i+4
  Ca646SynRWI[[i]] <- rwi.stats(test2Dat[[k]]$synchron$synChron, ids = ca646IDs)
}

Ca646SynRWI[seq(1,40,by=4)]
for(i in 1:13){
  cat(names(Ca646SynRWI[[1]])[i], mean(unlist(lapply(Ca646SynRWI, function(x) unlist(x)[[i]]))), "\n")
}
########################
#Get AR1 values
########################
test2Dat[[5]]$synchron$synChronData$origChronAR1
mean(unlist(lapply(test2Dat[5:44], function(x) x$synchron$synChronData$surChronAR1)))

########################
#Get correlations to target
########################
cor(chron646mv[as.numeric(rownames(chron646mv)) %in% climDat2[,1],1], climDat2[climDat2[,1] %in% as.numeric(rownames(chron646mv)),2])
mean(unlist(lapply(test2Dat[5:44], function(x) x$synchron$synChronData$surChronTargetCor)))

allCors <- rep(NA, 40)
for(i in 1:40){
  k <- i+4
  test2Dat[[k]]$synchron$synChronData
  synchron1 <- chron(test2Dat[[k]]$synchron$synChron)
  allCors[i] <- cor(synchron1[as.numeric(rownames(synchron1)) %in% climDat2[,1],1], climDat2[climDat2[,1] %in% as.numeric(rownames(synchron1)),2])
}
mean(allCors)

########################
#Correlations amoung synthetic and orginal chronology
########################
simIndex <- seq(5,41,by=4)
mvSynchrons <- data.frame(matrix(data=NA, ncol = 10, nrow = length(chron646mv[,1])))
for(i in 1:10){
  mvSynchrons[,i] <- chron(test2Dat[[simIndex[i]]]$synchron$synChron)[,1]
  cat(cor(chron646mv[,1], mvSynchrons[,i]), "\n")
}
All646Chrons <- cbind(chron646mv[,1], mvSynchrons)
names(All646Chrons)[1] <- c("Ca646")
for(i in 1:10){
  names(All646Chrons)[(i+1)] <- paste0("Sim",i)
}

cor(All646Chrons, All646Chrons)

########################
#Prediction Interval Testing
########################
lapply(test2Dat, function(x) x$runDetails)

cT1 <- test2Dat[[1]]$CaptureSummary
cT2 <- test2Dat[[2]]$CaptureSummary
cT3 <- test2Dat[[3]]$CaptureSummary
cT4 <- test2Dat[[4]]$CaptureSummary

#lapply(test2Dat, function(x) x$runDetails)[seq(24,44,2)]
ME5 <- seq(5,41,4)
ME9 <- seq(6,42,4)
trad5 <- seq(7,43,4)
trad9 <- seq(8,44,4)

a1 <- mean(unlist(lapply(test2Dat, function(x) x$CaptureSummary[1])[ME5]))
a2 <- mean(unlist(lapply(test2Dat, function(x) x$CaptureSummary[2])[ME5]))
a3 <- mean(unlist(lapply(test2Dat, function(x) x$CaptureSummary[3])[ME5]))
a4 <- mean(unlist(lapply(test2Dat, function(x) x$CaptureSummary[4])[ME5]))

b1 <- mean(unlist(lapply(test2Dat, function(x) x$CaptureSummary[1])[ME9]))
b2 <- mean(unlist(lapply(test2Dat, function(x) x$CaptureSummary[2])[ME9]))
b3 <- mean(unlist(lapply(test2Dat, function(x) x$CaptureSummary[3])[ME9]))
b4 <- mean(unlist(lapply(test2Dat, function(x) x$CaptureSummary[4])[ME9]))

c1 <- mean(unlist(lapply(test2Dat, function(x) x$CaptureSummary[1])[trad5]))
c2 <- mean(unlist(lapply(test2Dat, function(x) x$CaptureSummary[2])[trad5]))
c3 <- mean(unlist(lapply(test2Dat, function(x) x$CaptureSummary[3])[trad5]))
c4 <- mean(unlist(lapply(test2Dat, function(x) x$CaptureSummary[4])[trad5]))

d1 <- mean(unlist(lapply(test2Dat, function(x) x$CaptureSummary[1])[trad9]))
d2 <- mean(unlist(lapply(test2Dat, function(x) x$CaptureSummary[2])[trad9]))
d3 <- mean(unlist(lapply(test2Dat, function(x) x$CaptureSummary[3])[trad9]))
d4 <- mean(unlist(lapply(test2Dat, function(x) x$CaptureSummary[4])[trad9]))

summaryDat1 <- data.frame(row.names = c("ME50", "ME90", "trad50", "trad90"),
                          "ErrTheory" <- c(a1,b1,c1,d1),
                          "ErrEmp" <- c(a2,b2,c2,d2),
                          "ChronoConfEmp" <- c(a3,b3,c3,d3),
                          "ChronoConfTheory" <- c(a4,b4,c4,d4))

summaryDat1 <- rbind.data.frame(summaryDat1,cT1,cT2,cT3,cT4)
colnames(summaryDat1) <- c("ErrTheory","ErrEmp","ChronoConfTheory", "ChronoConfEmp")
rownames(summaryDat1) <- c("simME50", "simME90", "simTrad50", "simTrad90","realME50", "realME90", "realTrad50", "realTrad90")
summaryDat1


#######################################################################################################################
########################
#ca646 Sim2
########################
#######################################################################################################################

########################
#Summary Results Table
########################

test20Dat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test20.RData")

lapply(test20Dat, function(x) x$runDetails$Chronology)


########################
#Gather rwi stats
########################

Ca646SynRWI <- list()
for (i in 1:40){
  Ca646SynRWI[[i]] <- rwi.stats(test20Dat[[i]]$synchron$synChron, ids = ca646IDs)
}

Ca646SynRWI[seq(1,40,by=4)]
for(i in 1:13){
  cat(names(Ca646SynRWI[[1]])[i], mean(unlist(lapply(Ca646SynRWI, function(x) unlist(x)[[i]]))), "\n")
}

########################
#Get AR1 values
########################
test20Dat[[1]]$synchron$synChronData$climTargetAR1
test20Dat[[1]]$synchron$synChronData$origChronAR1
mean(unlist(lapply(test20Dat[1:40], function(x) x$synchron$synChronData$surChronAR1)))

########################
#Get correlations to target
########################
cor(chron646mv[as.numeric(rownames(chron646mv)) %in% climDat2[,1],1], climDat2[climDat2[,1] %in% as.numeric(rownames(chron646mv)),2])
mean(unlist(lapply(test20Dat[5:44], function(x) x$synchron$synChronData$surChronTargetCor)))

allCors <- rep(NA, 40)
for(i in 1:40){
  test20Dat[[i]]$synchron$synChronData
  synchron1 <- chron(test20Dat[[i]]$synchron$synChron)
  allCors[i] <- cor(synchron1[as.numeric(rownames(synchron1)) %in% climDat2[,1],1], climDat2[climDat2[,1] %in% as.numeric(rownames(synchron1)),2])
}
mean(allCors)

########################
#Prediction Interval Testing
########################
lapply(test20Dat, function(x) x$runDetails)


#lapply(test20Dat, function(x) x$runDetails)[ME5]
ME5 <- seq(1,37,4)
ME9 <- seq(2,38,4)
trad5 <- seq(3,39,4)
trad9 <- seq(4,40,4)

a1 <- mean(unlist(lapply(test20Dat, function(x) x$CaptureSummary[1])[ME5]))
a2 <- mean(unlist(lapply(test20Dat, function(x) x$CaptureSummary[2])[ME5]))
a3 <- mean(unlist(lapply(test20Dat, function(x) x$CaptureSummary[3])[ME5]))
a4 <- mean(unlist(lapply(test20Dat, function(x) x$CaptureSummary[4])[ME5]))

b1 <- mean(unlist(lapply(test20Dat, function(x) x$CaptureSummary[1])[ME9]))
b2 <- mean(unlist(lapply(test20Dat, function(x) x$CaptureSummary[2])[ME9]))
b3 <- mean(unlist(lapply(test20Dat, function(x) x$CaptureSummary[3])[ME9]))
b4 <- mean(unlist(lapply(test20Dat, function(x) x$CaptureSummary[4])[ME9]))

c1 <- mean(unlist(lapply(test20Dat, function(x) x$CaptureSummary[1])[trad5]))
c2 <- mean(unlist(lapply(test20Dat, function(x) x$CaptureSummary[2])[trad5]))
c3 <- mean(unlist(lapply(test20Dat, function(x) x$CaptureSummary[3])[trad5]))
c4 <- mean(unlist(lapply(test20Dat, function(x) x$CaptureSummary[4])[trad5]))

d1 <- mean(unlist(lapply(test20Dat, function(x) x$CaptureSummary[1])[trad9]))
d2 <- mean(unlist(lapply(test20Dat, function(x) x$CaptureSummary[2])[trad9]))
d3 <- mean(unlist(lapply(test20Dat, function(x) x$CaptureSummary[3])[trad9]))
d4 <- mean(unlist(lapply(test20Dat, function(x) x$CaptureSummary[4])[trad9]))

summaryDat1 <- data.frame(row.names = c("ME50", "ME90", "trad50", "trad90"),
                          "ErrTheory" <- c(a1,b1,c1,d1),
                          "ErrEmp" <- c(a2,b2,c2,d2),
                          "ChronoConfEmp" <- c(a3,b3,c3,d3),
                          "ChronoConfTheory" <- c(a4,b4,c4,d4))

colnames(summaryDat1) <- c("ErrTheory","ErrEmp","ChronoConfTheory", "ChronoConfEmp")
summaryDat1

#######################################################################################################################
########################
#ca646 Sim3
########################
#######################################################################################################################

########################
#Summary Results Table
########################

test21Dat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test21.RData")

lapply(test21Dat, function(x) x$runDetails$Chronology)



########################
#Gather rwi stats
########################

Ca646SynRWI <- list()
for (i in 1:40){
  Ca646SynRWI[[i]] <- rwi.stats(test21Dat[[i]]$synchron$synChron, ids = ca646IDs)
}

Ca646SynRWI[seq(1,40,by=4)]
for(i in 1:13){
  cat(names(Ca646SynRWI[[1]])[i], mean(unlist(lapply(Ca646SynRWI, function(x) unlist(x)[[i]]))), "\n")
}

########################
#Get AR1 values
########################
test21Dat[[1]]$synchron$synChronData$origChronAR1
mean(unlist(lapply(test21Dat[1:40], function(x) x$synchron$synChronData$surChronAR1)))

########################
#Get correlations to target
########################
cor(chron646mv[as.numeric(rownames(chron646mv)) %in% climDat2[,1],1], climDat2[climDat2[,1] %in% as.numeric(rownames(chron646mv)),2])
mean(unlist(lapply(test21Dat[5:44], function(x) x$synchron$synChronData$surChronTargetCor)))

allCors <- rep(NA, 40)
for(i in 1:40){
  test21Dat[[i]]$synchron$synChronData
  synchron1 <- chron(test21Dat[[i]]$synchron$synChron)
  allCors[i] <- cor(synchron1[as.numeric(rownames(synchron1)) %in% climDat2[,1],1], climDat2[climDat2[,1] %in% as.numeric(rownames(synchron1)),2])
}
mean(allCors)


########################
#Prediction Interval Testing
########################
lapply(test21Dat, function(x) x$runDetails)


#lapply(test21Dat, function(x) x$runDetails)[ME5]
ME5 <- seq(1,37,4)
ME9 <- seq(2,38,4)
trad5 <- seq(3,39,4)
trad9 <- seq(4,40,4)

a1 <- mean(unlist(lapply(test21Dat, function(x) x$CaptureSummary[1])[ME5]))
a2 <- mean(unlist(lapply(test21Dat, function(x) x$CaptureSummary[2])[ME5]))
a3 <- mean(unlist(lapply(test21Dat, function(x) x$CaptureSummary[3])[ME5]))
a4 <- mean(unlist(lapply(test21Dat, function(x) x$CaptureSummary[4])[ME5]))

b1 <- mean(unlist(lapply(test21Dat, function(x) x$CaptureSummary[1])[ME9]))
b2 <- mean(unlist(lapply(test21Dat, function(x) x$CaptureSummary[2])[ME9]))
b3 <- mean(unlist(lapply(test21Dat, function(x) x$CaptureSummary[3])[ME9]))
b4 <- mean(unlist(lapply(test21Dat, function(x) x$CaptureSummary[4])[ME9]))

c1 <- mean(unlist(lapply(test21Dat, function(x) x$CaptureSummary[1])[trad5]))
c2 <- mean(unlist(lapply(test21Dat, function(x) x$CaptureSummary[2])[trad5]))
c3 <- mean(unlist(lapply(test21Dat, function(x) x$CaptureSummary[3])[trad5]))
c4 <- mean(unlist(lapply(test21Dat, function(x) x$CaptureSummary[4])[trad5]))

d1 <- mean(unlist(lapply(test21Dat, function(x) x$CaptureSummary[1])[trad9]))
d2 <- mean(unlist(lapply(test21Dat, function(x) x$CaptureSummary[2])[trad9]))
d3 <- mean(unlist(lapply(test21Dat, function(x) x$CaptureSummary[3])[trad9]))
d4 <- mean(unlist(lapply(test21Dat, function(x) x$CaptureSummary[4])[trad9]))

summaryDat1 <- data.frame(row.names = c("ME50", "ME90", "trad50", "trad90"),
                          "ErrTheory" <- c(a1,b1,c1,d1),
                          "ErrEmp" <- c(a2,b2,c2,d2),
                          "ChronoConfEmp" <- c(a3,b3,c3,d3),
                          "ChronoConfTheory" <- c(a4,b4,c4,d4))

colnames(summaryDat1) <- c("ErrTheory","ErrEmp","ChronoConfTheory", "ChronoConfEmp")
summaryDat1

#######################################################################################################################
########################
#ca646 Sim4
########################
#######################################################################################################################

########################
#Summary Results Table
########################

test22Dat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test22.RData")

lapply(test22Dat, function(x) x$runDetails$Chronology)


########################
#Gather rwi stats
########################

Ca646SynRWI <- list()
for (i in 1:40){
  Ca646SynRWI[[i]] <- rwi.stats(test22Dat[[i]]$synchron$synChron, ids = ca646IDs)
}

Ca646SynRWI[seq(1,40,by=4)]
for(i in 1:13){
  cat(names(Ca646SynRWI[[1]])[i], mean(unlist(lapply(Ca646SynRWI, function(x) unlist(x)[[i]]))), "\n")
}

########################
#Get AR1 values
########################
test22Dat[[1]]$synchron$synChronData$origChronAR1
mean(unlist(lapply(test22Dat[1:40], function(x) x$synchron$synChronData$surChronAR1)))

########################
#Get correlations to target
########################
cor(chron646mv[as.numeric(rownames(chron646mv)) %in% climDat2[,1],1], climDat2[climDat2[,1] %in% as.numeric(rownames(chron646mv)),2])
mean(unlist(lapply(test22Dat[5:44], function(x) x$synchron$synChronData$surChronTargetCor)))

allCors <- rep(NA, 40)
for(i in 1:40){
  test22Dat[[i]]$synchron$synChronData
  synchron1 <- chron(test22Dat[[i]]$synchron$synChron)
  allCors[i] <- cor(synchron1[as.numeric(rownames(synchron1)) %in% climDat2[,1],1], climDat2[climDat2[,1] %in% as.numeric(rownames(synchron1)),2])
}
mean(allCors)


########################
#Prediction Interval Testing
########################
lapply(test22Dat, function(x) x$runDetails)


#lapply(test22Dat, function(x) x$runDetails)[ME5]
ME5 <- seq(1,37,4)
ME9 <- seq(2,38,4)
trad5 <- seq(3,39,4)
trad9 <- seq(4,40,4)

a1 <- mean(unlist(lapply(test22Dat, function(x) x$CaptureSummary[1])[ME5]))
a2 <- mean(unlist(lapply(test22Dat, function(x) x$CaptureSummary[2])[ME5]))
a3 <- mean(unlist(lapply(test22Dat, function(x) x$CaptureSummary[3])[ME5]))
a4 <- mean(unlist(lapply(test22Dat, function(x) x$CaptureSummary[4])[ME5]))

b1 <- mean(unlist(lapply(test22Dat, function(x) x$CaptureSummary[1])[ME9]))
b2 <- mean(unlist(lapply(test22Dat, function(x) x$CaptureSummary[2])[ME9]))
b3 <- mean(unlist(lapply(test22Dat, function(x) x$CaptureSummary[3])[ME9]))
b4 <- mean(unlist(lapply(test22Dat, function(x) x$CaptureSummary[4])[ME9]))

c1 <- mean(unlist(lapply(test22Dat, function(x) x$CaptureSummary[1])[trad5]))
c2 <- mean(unlist(lapply(test22Dat, function(x) x$CaptureSummary[2])[trad5]))
c3 <- mean(unlist(lapply(test22Dat, function(x) x$CaptureSummary[3])[trad5]))
c4 <- mean(unlist(lapply(test22Dat, function(x) x$CaptureSummary[4])[trad5]))

d1 <- mean(unlist(lapply(test22Dat, function(x) x$CaptureSummary[1])[trad9]))
d2 <- mean(unlist(lapply(test22Dat, function(x) x$CaptureSummary[2])[trad9]))
d3 <- mean(unlist(lapply(test22Dat, function(x) x$CaptureSummary[3])[trad9]))
d4 <- mean(unlist(lapply(test22Dat, function(x) x$CaptureSummary[4])[trad9]))

summaryDat1 <- data.frame(row.names = c("ME50", "ME90", "trad50", "trad90"),
                          "ErrTheory" <- c(a1,b1,c1,d1),
                          "ErrEmp" <- c(a2,b2,c2,d2),
                          "ChronoConfEmp" <- c(a3,b3,c3,d3),
                          "ChronoConfTheory" <- c(a4,b4,c4,d4))

colnames(summaryDat1) <- c("ErrTheory","ErrEmp","ChronoConfTheory", "ChronoConfEmp")
summaryDat1


#######################################################################################################################
#######################################################################################################################
#######################################################################################################################

#Cana113 Mountain Hemlock


#load cana113 TSME MXD data
climDat4 <- read.csv("C:/Users/dce72/Documents/GradSchool/Uncertainty/SimpleBootTrials/canaTempTarget.csv")
head(climDat4)
climDat4 <- climDat4[,-1]
names(climDat4) <- c("Year", "Target")
chron113 <- read.rwl("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\SimpleBootTrials\\cana113.rwl")
chron113mv <- chron(chron113)
cor(chron113mv[as.numeric(rownames(chron113mv)) %in% climDat4[,1],1], climDat4[climDat4[,1] %in% as.numeric(rownames(chron113mv)),2])
rwi.stats(chron113)


colnames(chron113)

#######################################################################################################################
########################
#cana113 Mountain Hemlock MXD
#cana113 Sim1
########################
#######################################################################################################################

########################
#Summary Results Table
########################

test4Dat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test4.RData")

lapply(test4Dat, function(x) x$runDetails$Chronology)


########################
#Gather rwi stats
########################

Cana113SynRWI <- list()
for (i in 1:40){
  k=i+4
  Cana113SynRWI[[i]] <- rwi.stats(test4Dat[[k]]$synchron$synChron)
}

Cana113SynRWI[seq(1,40,by=4)]
for(i in 1:13){
  cat(names(Cana113SynRWI[[1]])[i], mean(unlist(lapply(Cana113SynRWI, function(x) unlist(x)[[i]]))), "\n")
}
########################
#Get AR1 values
########################
test4Dat[[5]]$synchron$synChronData$origChronAR1
mean(unlist(lapply(test4Dat[5:44], function(x) x$synchron$synChronData$surChronAR1)))

########################
#Get correlations to target
########################
cor(chron113mv[as.numeric(rownames(chron113mv)) %in% climDat4[,1],1], climDat4[climDat4[,1] %in% as.numeric(rownames(chron113mv)),2])
mean(unlist(lapply(test4Dat[5:44], function(x) x$synchron$synChronData$surChronTargetCor)))

allCors <- rep(NA, 40)
for(i in 1:40){
  k <- i+4
  test4Dat[[k]]$synchron$synChronData
  synchron1 <- chron(test4Dat[[k]]$synchron$synChron)
  allCors[i] <- cor(synchron1[as.numeric(rownames(synchron1)) %in% climDat4[,1],1], climDat4[climDat4[,1] %in% as.numeric(rownames(synchron1)),2])
}
mean(allCors)

########################
#Correlations amoung synthetic and orginal chronology
########################
simIndex <- seq(5,41,by=4)
mvSynchrons <- data.frame(matrix(data=NA, ncol = 10, nrow = length(chron113mv[,1])))
for(i in 1:10){
  mvSynchrons[,i] <- chron(test4Dat[[simIndex[i]]]$synchron$synChron)[,1]
  cat(cor(chron113mv[,1], mvSynchrons[,i]), "\n")
}
All113Chrons <- cbind(chron113mv[,1], mvSynchrons)
names(All113Chrons)[1] <- c("Ca646")
for(i in 1:10){
  names(All113Chrons)[(i+1)] <- paste0("Sim",i)
}

cor(All113Chrons, All113Chrons)

########################
#Prediction Interval Testing
########################
lapply(test4Dat, function(x) x$runDetails)

cT1 <- test4Dat[[1]]$CaptureSummary
cT2 <- test4Dat[[2]]$CaptureSummary
cT3 <- test4Dat[[3]]$CaptureSummary
cT4 <- test4Dat[[4]]$CaptureSummary

#lapply(test4Dat, function(x) x$runDetails)[ME5]
ME5 <- seq(5,41,4)
ME9 <- seq(6,42,4)
trad5 <- seq(7,43,4)
trad9 <- seq(8,44,4)

a1 <- mean(unlist(lapply(test4Dat, function(x) x$CaptureSummary[1])[ME5]))
a2 <- mean(unlist(lapply(test4Dat, function(x) x$CaptureSummary[2])[ME5]))
a3 <- mean(unlist(lapply(test4Dat, function(x) x$CaptureSummary[3])[ME5]))
a4 <- mean(unlist(lapply(test4Dat, function(x) x$CaptureSummary[4])[ME5]))

b1 <- mean(unlist(lapply(test4Dat, function(x) x$CaptureSummary[1])[ME9]))
b2 <- mean(unlist(lapply(test4Dat, function(x) x$CaptureSummary[2])[ME9]))
b3 <- mean(unlist(lapply(test4Dat, function(x) x$CaptureSummary[3])[ME9]))
b4 <- mean(unlist(lapply(test4Dat, function(x) x$CaptureSummary[4])[ME9]))

c1 <- mean(unlist(lapply(test4Dat, function(x) x$CaptureSummary[1])[trad5]))
c2 <- mean(unlist(lapply(test4Dat, function(x) x$CaptureSummary[2])[trad5]))
c3 <- mean(unlist(lapply(test4Dat, function(x) x$CaptureSummary[3])[trad5]))
c4 <- mean(unlist(lapply(test4Dat, function(x) x$CaptureSummary[4])[trad5]))

d1 <- mean(unlist(lapply(test4Dat, function(x) x$CaptureSummary[1])[trad9]))
d2 <- mean(unlist(lapply(test4Dat, function(x) x$CaptureSummary[2])[trad9]))
d3 <- mean(unlist(lapply(test4Dat, function(x) x$CaptureSummary[3])[trad9]))
d4 <- mean(unlist(lapply(test4Dat, function(x) x$CaptureSummary[4])[trad9]))

summaryDat1 <- data.frame(row.names = c("ME50", "ME90", "trad50", "trad90"),
                          "ErrTheory" <- c(a1,b1,c1,d1),
                          "ErrEmp" <- c(a2,b2,c2,d2),
                          "ChronoConfEmp" <- c(a3,b3,c3,d3),
                          "ChronoConfTheory" <- c(a4,b4,c4,d4))

summaryDat1 <- rbind.data.frame(summaryDat1,cT1,cT2,cT3,cT4)
colnames(summaryDat1) <- c("ErrTheory","ErrEmp","ChronoConfTheory", "ChronoConfEmp")
rownames(summaryDat1) <- c("simME50", "simME90", "simTrad50", "simTrad90","realME50", "realME90", "realTrad50", "realTrad90")
summaryDat1

#######################################################################################################################
########################
#cana113 Sim2
########################
#######################################################################################################################


test40Dat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test40.RData")

lapply(test40Dat, function(x) x$runDetails$Chronology)


########################
#Gather rwi stats
########################

Cana113SynRWI <- list()
for (i in 1:40){

  Cana113SynRWI[[i]] <- rwi.stats(test40Dat[[i]]$synchron$synChron)
}

Cana113SynRWI[seq(1,37,by=4)]
for(i in 1:13){
  cat(names(Cana113SynRWI[[1]])[i], mean(unlist(lapply(Cana113SynRWI, function(x) unlist(x)[[i]]))), "\n")
}
########################
#Get AR1 values
########################
test40Dat[[5]]$synchron$synChronData$origChronAR1
mean(unlist(lapply(test40Dat[5:44], function(x) x$synchron$synChronData$surChronAR1)))

########################
#Get correlations to target
########################
cor(chron113mv[as.numeric(rownames(chron113mv)) %in% climDat4[,1],1], climDat4[climDat4[,1] %in% as.numeric(rownames(chron113mv)),2])
mean(unlist(lapply(test40Dat[5:44], function(x) x$synchron$synChronData$surChronTargetCor)))

allCors <- rep(NA, 40)
for(i in 1:40){
  test40Dat[[i]]$synchron$synChronData
  synchron1 <- chron(test40Dat[[i]]$synchron$synChron)
  allCors[i] <- cor(synchron1[as.numeric(rownames(synchron1)) %in% climDat4[,1],1], climDat4[climDat4[,1] %in% as.numeric(rownames(synchron1)),2])
}
mean(allCors)

########################
#Prediction Interval Testing
########################
lapply(test40Dat, function(x) x$runDetails)


#lapply(test40Dat, function(x) x$runDetails)[ME5]
ME5 <- seq(1,37,4)
ME9 <- seq(2,38,4)
trad5 <- seq(3,39,4)
trad9 <- seq(4,40,4)

a1 <- mean(unlist(lapply(test40Dat, function(x) x$CaptureSummary[1])[ME5]))
a2 <- mean(unlist(lapply(test40Dat, function(x) x$CaptureSummary[2])[ME5]))
a3 <- mean(unlist(lapply(test40Dat, function(x) x$CaptureSummary[3])[ME5]))
a4 <- mean(unlist(lapply(test40Dat, function(x) x$CaptureSummary[4])[ME5]))

b1 <- mean(unlist(lapply(test40Dat, function(x) x$CaptureSummary[1])[ME9]))
b2 <- mean(unlist(lapply(test40Dat, function(x) x$CaptureSummary[2])[ME9]))
b3 <- mean(unlist(lapply(test40Dat, function(x) x$CaptureSummary[3])[ME9]))
b4 <- mean(unlist(lapply(test40Dat, function(x) x$CaptureSummary[4])[ME9]))

c1 <- mean(unlist(lapply(test40Dat, function(x) x$CaptureSummary[1])[trad5]))
c2 <- mean(unlist(lapply(test40Dat, function(x) x$CaptureSummary[2])[trad5]))
c3 <- mean(unlist(lapply(test40Dat, function(x) x$CaptureSummary[3])[trad5]))
c4 <- mean(unlist(lapply(test40Dat, function(x) x$CaptureSummary[4])[trad5]))

d1 <- mean(unlist(lapply(test40Dat, function(x) x$CaptureSummary[1])[trad9]))
d2 <- mean(unlist(lapply(test40Dat, function(x) x$CaptureSummary[2])[trad9]))
d3 <- mean(unlist(lapply(test40Dat, function(x) x$CaptureSummary[3])[trad9]))
d4 <- mean(unlist(lapply(test40Dat, function(x) x$CaptureSummary[4])[trad9]))

summaryDat1 <- data.frame(row.names = c("ME50", "ME90", "trad50", "trad90"),
                          "ErrTheory" <- c(a1,b1,c1,d1),
                          "ErrEmp" <- c(a2,b2,c2,d2),
                          "ChronoConfEmp" <- c(a3,b3,c3,d3),
                          "ChronoConfTheory" <- c(a4,b4,c4,d4))

colnames(summaryDat1) <- c("ErrTheory","ErrEmp","ChronoConfTheory", "ChronoConfEmp")
summaryDat1

#######################################################################################################################
########################
#cana113 Sim3
########################
#######################################################################################################################


test41Dat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test41.RData")

lapply(test41Dat, function(x) x$runDetails$Chronology)


########################
#Gather rwi stats
########################

Cana113SynRWI <- list()
for (i in 1:40){
  
  Cana113SynRWI[[i]] <- rwi.stats(test41Dat[[i]]$synchron$synChron)
}

Cana113SynRWI[seq(1,37,by=4)]
for(i in 1:13){
  cat(names(Cana113SynRWI[[1]])[i], mean(unlist(lapply(Cana113SynRWI, function(x) unlist(x)[[i]]))), "\n")
}
########################
#Get AR1 values
########################
test41Dat[[5]]$synchron$synChronData$origChronAR1
mean(unlist(lapply(test41Dat[5:44], function(x) x$synchron$synChronData$surChronAR1)))

########################
#Get correlations to target
########################
cor(chron113mv[as.numeric(rownames(chron113mv)) %in% climDat4[,1],1], climDat4[climDat4[,1] %in% as.numeric(rownames(chron113mv)),2])
mean(unlist(lapply(test41Dat[5:44], function(x) x$synchron$synChronData$surChronTargetCor)))

allCors <- rep(NA, 40)
for(i in 1:40){
  test41Dat[[i]]$synchron$synChronData
  synchron1 <- chron(test41Dat[[i]]$synchron$synChron)
  allCors[i] <- cor(synchron1[as.numeric(rownames(synchron1)) %in% climDat4[,1],1], climDat4[climDat4[,1] %in% as.numeric(rownames(synchron1)),2])
}
mean(allCors)

########################
#Prediction Interval Testing
########################
lapply(test41Dat, function(x) x$runDetails)


#lapply(test41Dat, function(x) x$runDetails)[ME5]
ME5 <- seq(1,37,4)
ME9 <- seq(2,38,4)
trad5 <- seq(3,39,4)
trad9 <- seq(4,40,4)

a1 <- mean(unlist(lapply(test41Dat, function(x) x$CaptureSummary[1])[ME5]))
a2 <- mean(unlist(lapply(test41Dat, function(x) x$CaptureSummary[2])[ME5]))
a3 <- mean(unlist(lapply(test41Dat, function(x) x$CaptureSummary[3])[ME5]))
a4 <- mean(unlist(lapply(test41Dat, function(x) x$CaptureSummary[4])[ME5]))

b1 <- mean(unlist(lapply(test41Dat, function(x) x$CaptureSummary[1])[ME9]))
b2 <- mean(unlist(lapply(test41Dat, function(x) x$CaptureSummary[2])[ME9]))
b3 <- mean(unlist(lapply(test41Dat, function(x) x$CaptureSummary[3])[ME9]))
b4 <- mean(unlist(lapply(test41Dat, function(x) x$CaptureSummary[4])[ME9]))

c1 <- mean(unlist(lapply(test41Dat, function(x) x$CaptureSummary[1])[trad5]))
c2 <- mean(unlist(lapply(test41Dat, function(x) x$CaptureSummary[2])[trad5]))
c3 <- mean(unlist(lapply(test41Dat, function(x) x$CaptureSummary[3])[trad5]))
c4 <- mean(unlist(lapply(test41Dat, function(x) x$CaptureSummary[4])[trad5]))

d1 <- mean(unlist(lapply(test41Dat, function(x) x$CaptureSummary[1])[trad9]))
d2 <- mean(unlist(lapply(test41Dat, function(x) x$CaptureSummary[2])[trad9]))
d3 <- mean(unlist(lapply(test41Dat, function(x) x$CaptureSummary[3])[trad9]))
d4 <- mean(unlist(lapply(test41Dat, function(x) x$CaptureSummary[4])[trad9]))

summaryDat1 <- data.frame(row.names = c("ME50", "ME90", "trad50", "trad90"),
                          "ErrTheory" <- c(a1,b1,c1,d1),
                          "ErrEmp" <- c(a2,b2,c2,d2),
                          "ChronoConfEmp" <- c(a3,b3,c3,d3),
                          "ChronoConfTheory" <- c(a4,b4,c4,d4))

colnames(summaryDat1) <- c("ErrTheory","ErrEmp","ChronoConfTheory", "ChronoConfEmp")
summaryDat1

#######################################################################################################################
########################
#cana113 Sim4
########################
#######################################################################################################################


test42Dat <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test42.RData")

lapply(test42Dat, function(x) x$runDetails$Chronology)


########################
#Gather rwi stats
########################

Cana113SynRWI <- list()
for (i in 1:40){
  
  Cana113SynRWI[[i]] <- rwi.stats(test42Dat[[i]]$synchron$synChron)
}

Cana113SynRWI[seq(1,37,by=4)]
for(i in 1:13){
  cat(names(Cana113SynRWI[[1]])[i], mean(unlist(lapply(Cana113SynRWI, function(x) unlist(x)[[i]]))), "\n")
}
########################
#Get AR1 values
########################
test42Dat[[5]]$synchron$synChronData$origChronAR1
mean(unlist(lapply(test42Dat[5:44], function(x) x$synchron$synChronData$surChronAR1)))

########################
#Get correlations to target
########################
cor(chron113mv[as.numeric(rownames(chron113mv)) %in% climDat4[,1],1], climDat4[climDat4[,1] %in% as.numeric(rownames(chron113mv)),2])
mean(unlist(lapply(test42Dat[5:44], function(x) x$synchron$synChronData$surChronTargetCor)))

allCors <- rep(NA, 40)
for(i in 1:40){
  test42Dat[[i]]$synchron$synChronData
  synchron1 <- chron(test42Dat[[i]]$synchron$synChron)
  allCors[i] <- cor(synchron1[as.numeric(rownames(synchron1)) %in% climDat4[,1],1], climDat4[climDat4[,1] %in% as.numeric(rownames(synchron1)),2])
}
mean(allCors)

########################
#Prediction Interval Testing
########################
lapply(test42Dat, function(x) x$runDetails)


#lapply(test42Dat, function(x) x$runDetails)[ME5]
ME5 <- seq(1,37,4)
ME9 <- seq(2,38,4)
trad5 <- seq(3,39,4)
trad9 <- seq(4,40,4)

a1 <- mean(unlist(lapply(test42Dat, function(x) x$CaptureSummary[1])[ME5]))
a2 <- mean(unlist(lapply(test42Dat, function(x) x$CaptureSummary[2])[ME5]))
a3 <- mean(unlist(lapply(test42Dat, function(x) x$CaptureSummary[3])[ME5]))
a4 <- mean(unlist(lapply(test42Dat, function(x) x$CaptureSummary[4])[ME5]))

b1 <- mean(unlist(lapply(test42Dat, function(x) x$CaptureSummary[1])[ME9]))
b2 <- mean(unlist(lapply(test42Dat, function(x) x$CaptureSummary[2])[ME9]))
b3 <- mean(unlist(lapply(test42Dat, function(x) x$CaptureSummary[3])[ME9]))
b4 <- mean(unlist(lapply(test42Dat, function(x) x$CaptureSummary[4])[ME9]))

c1 <- mean(unlist(lapply(test42Dat, function(x) x$CaptureSummary[1])[trad5]))
c2 <- mean(unlist(lapply(test42Dat, function(x) x$CaptureSummary[2])[trad5]))
c3 <- mean(unlist(lapply(test42Dat, function(x) x$CaptureSummary[3])[trad5]))
c4 <- mean(unlist(lapply(test42Dat, function(x) x$CaptureSummary[4])[trad5]))

d1 <- mean(unlist(lapply(test42Dat, function(x) x$CaptureSummary[1])[trad9]))
d2 <- mean(unlist(lapply(test42Dat, function(x) x$CaptureSummary[2])[trad9]))
d3 <- mean(unlist(lapply(test42Dat, function(x) x$CaptureSummary[3])[trad9]))
d4 <- mean(unlist(lapply(test42Dat, function(x) x$CaptureSummary[4])[trad9]))

summaryDat1 <- data.frame(row.names = c("ME50", "ME90", "trad50", "trad90"),
                          "ErrTheory" <- c(a1,b1,c1,d1),
                          "ErrEmp" <- c(a2,b2,c2,d2),
                          "ChronoConfEmp" <- c(a3,b3,c3,d3),
                          "ChronoConfTheory" <- c(a4,b4,c4,d4))

colnames(summaryDat1) <- c("ErrTheory","ErrEmp","ChronoConfTheory", "ChronoConfEmp")
summaryDat1


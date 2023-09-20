#I am building a reconstruction and I want to add prediction intervals
#I have detrended the chronology
#I have chosen the climate target


#clear workspace
rm(list=ls())
gc()
#Set up environment
uncertaintiesDir <- "F:/Lab Backup/Uncertainty/"
#load package(s)
require(dplR)
require(forecast)
devtools::load_all(paste0(uncertaintiesDir,"R"))
set.seed(1348)

#load ca646 Blue Oak detrended ring width data
chron646 <- read.rwl(paste0(uncertaintiesDir,"SimpleBootTrials\\ca646rwi.csv"))
chron646mv <- chron(chron646)


#load and organize climate target
climDat2 <- read.csv(paste0(uncertaintiesDir,"SimpleBootTrials/ca646PrecipTarget.csv"))
climDat2 <- climDat2[,-1]
names(climDat2) <- c("Year", "Target")

#Quick check of relationship
cor(chron646mv[as.numeric(rownames(chron646mv)) %in% climDat2[,1],1], climDat2[climDat2[,1] %in% as.numeric(rownames(chron646mv)),2])

#Choose the parameters of the prediction interval test
#Use the Rock Springs Ranch chronology and Prism Precip data
#In addition to testing the original chronology, run 5 simulations
#Use Maximum Entropy Bootstrapping with 100 iterations
#Build 90% prediction intervals
#Alter the sample depth in 50% of simulations
chron646Test <- fullTestingIntervalsNew(
  chronology = chron646,
  climDat = climDat2,
  totalSims = 1,
  bootstappingMethod = "ME",
  bootIts = 100,
  predictionEnvelope = 0.9,
  skipChron=FALSE,
  alterSampleDepth=0.5)


#plot the regression assumptions testing
library(reshape2)
library(ggplot2)
library(tibble)
library(ggbeeswarm)

regressionCheck <- plotRegAssumpt(chron646Test)

#We see that one reconstruction fails the normaility assumption and another fails the test of homoscedasticity
#If we used multiple prediction interval envelopes, we would see different colors of points for each
#Similarly if we specified both "ME" and "Trad" bootstrapping, we would see two facets two display all the data

#this function also produces a table
#let's take a look
regressionCheck[[1]]
#so the firt simulation (row 2) fails normality
#and the third (row 4) fails homoscedasticity

#Now let's have a look at the chronology properties

props <- plotProps(chron646, climDat2, chron646Test)

#We can see that the simulated chronologies have similar properties to the original

#again, we can look at the values in a table
props[[1]]

#It looks like our simulations generally have lower correlations with the climate
#target than the original chronology does
#We may want to run more simulations to capture this property better

#Now let's see the performance of the prediction intervals

plotCapture(chron646Test)

#It looks like the 90% emprical PIs with MEboot do a good job,
#and so do the theoretical PIs for this chronology
#The original chronology tends to over-capture relative to the 5 simulations
#If we add "traditional bootstrapping" and another PI envelope to our test,
#we'll set more panels in this plot


#I think I would stick with empircial MEboot PIs for this chronology
#Here's how we grab those

setAsideUpper7 <- median(calibDat[,13,setAsideLoop]) + median(calibDat[,14,setAsideLoop]) * reconDat$Chronology[setAsideIndex2] + median(calibDat[,10,setAsideLoop])
setAsideLower7 <- median(calibDat[,15,setAsideLoop]) + median(calibDat[,16,setAsideLoop]) * reconDat$Chronology[setAsideIndex2] - median(calibDat[,10,setAsideLoop])





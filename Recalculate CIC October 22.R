
#OLD
test1RealDatOLD <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\TreeNobTest.RData")
test2RealDatOLD <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\ca646Test.RData")
test4RealDatOLD <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\cana113test.RData")
test1DatOLD <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\TreeNobSimTest.RData")
test2DatOLD <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\ca646SimTest.RData")
test4DatOLD <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\cana113SimTest.RData")

#NEW
test1RealDatNEW <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\TreeNobTest2.RData")
test2RealDatNEW <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\ca646Test2.RData")
test4RealDatNEW <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\cana113test2.RData")
test1DatNEW <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\TreeNobSimTest2.RData")
test2DatNEW <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\ca646SimTest2.RData")
test4DatNEW <- readRDS("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\cana113SimTest2.RData")

allData <- c("test1RealDatOLD", "test4RealDatOLD", "test4RealDatOLD", "test1DatOLD", "test2DatOLD", "test4DatOLD",
             "test1RealDatNEW", "test2RealDatNEW", "test4RealDatNEW", "test1DatNEW", "test2DatNEW", "test4DatNEW")

# tests <- list()
# testNum <- 0
# for (i in 1:length(allData)){
#   testNow <- eval(as.name(allData[i]))
#
#   for (k in 1:length(testNow)){
#     testNum <- testNum + 1
#     tests[[testNum]] <- predIntTest(testNow[[k]]$FullData$reconDat, envelope = testNow[[k]]$runDetails$`Prediction Interval`)
#
#   }
# }
#
# saveRDS(tests, file = "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\NewCICvalues_October22.rds")


tests <- list()
testNum <- 0
for (i in 1:length(allData)){
  testNow <- eval(as.name(allData[i]))

  for (k in 1:length(testNow)){
    testNum <- testNum + 1
    testNow[[k]]$runDetails$ChronNumber <- floor((testNum-1)/4)+1
    tests[[testNum]] <- testNow[[k]]$runDetails

  }
}

saveRDS(tests, file = "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\NewCICvalues_October22_rundetails.rds")


length(tests)


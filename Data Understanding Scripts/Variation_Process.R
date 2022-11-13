########################################################################################
## Script for automating the process of variation analysis
## -------------------------------------------------------------------------------------
## Copyright (c) 2022 F. Javier Nieto. All rights reserved.
## This file is part of my PhD work.
##
## This script is free software: you can redistribute it and/or modify it
## under the terms of the Apache License, Version 2.0 (the License) License.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT ANY WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT, IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
## OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.
##
## See README file for full disclaimer information and LICENSE file for full
## license information in the project root.
##
## @author: Francisco Javier Nieto
## e-mail: fj.nieto.es@gmail.com, fjavier.nieto@opendeusto.es, francisco.nieto@atos.net
#######################################################################################

library(randtests)
library(EnvStats)
library(nortest)
library(fitdistrplus)
library(foreach)
library(doParallel)

############################
# Configuration parameters #
############################

absoluteZero <- TRUE
# inputFile <- 'C:/PhD/Arduino/station_arduino.csv'
# inputFile <- 'C:/PhD/HiDALGO_Bosch/1WeekAirQualityBosch.csv'
inputFile <- 'C:/PhD/Movil Toolbox/Luminosity_log_2020-07-15_night_indoor_corrected.csv'


# Function that looks for the best distribution fitting the current dataset
distribution_fit <- function (sensorData) {
  
  # If there are values below 0, we move the dataset up --> Necessary for weibull, gamma...
  minValue <- min(sensorData)
  if (minValue<0) {
    sensorData <- sensorData + ((minValue*-1) +1)
  }
  # Initialize AIC and BIC lists
  aicList <- c(9999, 9999, 9999, 9999, 9999)
  bicList <- c(9999, 9999, 9999, 9999, 9999)
  
  # Select the fitdist with mge in case the standard (mle) gives an error in the optimization
  # Calculate fit for 5 distributions: weibull, lognormal, gamma, normal and uniform
  # fweibull <- fitdist(sensorData, "weibull", method="mge")
  fweibull <- fitdist(sensorData, "weibull")
  if (!is.null(fweibull)) {
    # print(paste("Weibull AIC: ", fweibull$aic))
    # print(paste("Weibull BIC: ", fweibull$bic))
    aicList[1] <- fweibull$aic
    bicList[1] <- fweibull$bic
  }
        
  # fgamma <- fitdist(sensorData, "gamma", method="mge")
  fgamma <- fitdist(sensorData, "gamma")
  if (!is.null(fgamma)) {  
    # print(paste("Gamma AIC: ", fgamma$aic))
    # print(paste("Gamma BIC: ", fgamma$bic))
    aicList[2] <- fgamma$aic
    bicList[2] <- fgamma$bic
  }
  
  # flnorm <- fitdist(sensorData, "lnorm", method="mge")
  flnorm <- fitdist(sensorData, "lnorm")
  if (!is.null(flnorm)) {
    # print(paste("Lnorm AIC: ", flnorm$aic))
    # print(paste("Lnorm BIC: ", flnorm$bic))
    aicList[3] <- flnorm$aic
    bicList[3] <- flnorm$bic
  }
     
  # fnorm <- fitdist(sensorData, "norm", method="mge")
  fnorm <- fitdist(sensorData, "norm")
  if (!is.null(fnorm)) {
    # print(paste("Norm AIC: ", fnorm$aic))
    # print(paste("Norm BIC: ", fnorm$bic))
    aicList[4] <- fnorm$aic
    bicList[4] <- fnorm$bic
  }
  
  # funif <- fitdist(sensorData, "unif", method="mge")
  funif <- fitdist(sensorData, "unif")
  if (!is.null(funif)) {
    # print(paste("Unif AIC: ", funif$aic))
    # print(paste("Unif BIC: ", funif$bic))
    aicList[5] <- funif$aic
    bicList[5] <- funif$bic
  }
  
  fullGofList <- aicList + bicList
  
  # Revise the results and indicate the one with best fit
  bestGofFit <- which.min(fullGofList)
  distributionResult <- switch(bestGofFit, '1' = {"weibull"}, '2' = {"gamma"}, '3' = {"lnorm"}, '4' = {"norm"}, '5' = {"unif"})
  return (distributionResult)
}


# Function that calculates all the tests --> So its execution can be parallelized
variation_analysis <- function (sensorData, iteration, abZero) {
  
    # Create a plot with the data 
  title <- paste ("Iteration", iteration)
  #plot(sensorData, col="green", main=title)
  #print(title)
  
  # Calculate coefficient of variation
  # coefVar <- NaN
  coefVar <- -100
  if (abZero) {
    coefVar <- cv(sensorData)
  }
  #print(paste("coefVar: ", coefVar))
  
  # Check normality of the data 
  shapiroValue <- NaN
  adValue <- NaN
  runsVal <- NaN
  boxVal <- NaN
  # Bear in mind that, if all values are equal, the tests fail
  if (length(unique(sensorData))!=1) {
    # Check normality of the data with Shapiro-Wilk
    normalityTest1 <- shapiro.test(sensorData)
    shapiroValue <- getElement(normalityTest1, "p.value")
    
    # Check normality of the data with Anderson-Darling
    normalityTest2 <- ad.test(sensorData)
    adValue <- getElement(normalityTest2, "p.value")
    
    # Check if we have random data
    # Runs test with mean as reference (median can be used)
    myThreshold <- mean(sensorData)
    outRuns = runs.test(sensorData, threshold = myThreshold)
    runsVal <- getElement(outRuns, "p.value")
    
    # Ljung-Box test (autocorrelation test that gives idea about white noise time-series)
    outBox = Box.test(sensorData, lag = 1, type = "Ljung")
    boxVal <- getElement(outBox, "p.value")
    # print(outBox)
    
  }
  #print(paste("Shapiro: ", shapiroValue))
  #print(paste("AD: ", adValue))
  #print(paste("Runs: ", runsVal))
  
  # Normality consensus and calculate mean and variance
  normalityAgreed <- TRUE
  bestFit <- NaN
  bestFitName <- "None"
  #print("Determining distribution...")
  if (is.nan(shapiroValue) | is.nan(adValue)) {
    normalityAgreed <- FALSE
    bestFitName <- "one-point"
    #print(paste("Distr: ", bestFitName))
  } else if (shapiroValue<0.05 | adValue<0.05) {
    normalityAgreed <- FALSE
    # Calculate best fit of the dataset for 5 distributions: weibull, lognormal, gamma, normal, uniform
    bestFitName <- distribution_fit(sensorData)
  } else {
    bestFitName <- "norm"
  }
  
  # print(paste("Distr: ", bestFitName))
  
  # randomData <- FALSE
  # if (outRuns$p.value > 0.05) {
  #   randomData <- TRUE
  # }
  
  # Calculate quartiles and IQR
  # outQuantile=quantile(sensorData)
  #print(outQuantile)
  outIQR <- IQR(sensorData)
  #print(paste("IQR: ", outIQR))
  
  # Prepare result of the function
  myReturnList <- list ("cv" = coefVar, "shapiro" = shapiroValue, "ad" = adValue, 
                        "normality" = normalityAgreed, "bestDistFit" = bestFitName,
                        "runs" = runsVal, "ljungBox" = boxVal, "IQR" = outIQR)
  return(myReturnList)
}

# Main code of the process
# t1 <- system.time({
# Load the file with the data
fullDataset <- read.csv(inputFile, sep = ";", header = TRUE)

# Extract the data we are interested in and plot the full dataset
sensorDataFull <- fullDataset[,2]
plot(sensorDataFull, col="green", main="Full dataset")

# Define the long data chunks with sliding windows of 50 steps
numMetrics <- length(sensorDataFull)
longWindowLength <- round (numMetrics*0.06, 0)
referenceIndexesLong <- sample (longWindowLength:(numMetrics-50),12,replace=F)
referenceIndexesLong

# Set up the parallel environment PSOCK cluster, so it can be used in Windows and Linux
defaultCores <- detectCores()-2
defaultCores
myCluster <- makeCluster(defaultCores, type = "PSOCK")
myCluster
registerDoParallel(cl=myCluster)
# })

# Iterate through the data chunks with long windows
# t2 <- system.time({
  fullResultLong <- foreach (i=1:12, .combine = 'cbind', .packages=c("nortest", "randtests", "foreach", "fitdistrplus", "EnvStats")) %dopar% {
    currentIndex <- referenceIndexesLong[i]
    
    # Prepare the sliding windows (50 steps)
    partialResult <- foreach (j=1:50, .combine = 'cbind') %do% {
      initialIndex <- currentIndex+j-1-longWindowLength+j-1
      finalIndex <- currentIndex+j-1
      sensorDataChunk <- sensorDataFull [initialIndex:finalIndex] 
      resultSlidingWindow <- variation_analysis(sensorDataChunk, j, absoluteZero)
      
      # Extract results for outcome preparation
      coefVar <- getElement(resultSlidingWindow, "cv")
      shapiroVal <- getElement(resultSlidingWindow, "shapiro")
      adVal <- getElement(resultSlidingWindow, "ad")
      normalityAgreed <- getElement(resultSlidingWindow, "normality")
      bestFitDistr <- getElement(resultSlidingWindow, "bestDistFit")
      runsVal <- getElement(resultSlidingWindow, "runs")
      boxVal <- getElement(resultSlidingWindow, "ljungBox")
      outIQR <- getElement(resultSlidingWindow, "IQR")
      
      # Leave the results in a list, in memory, for combination with the other loops results
      myFullReturnList <- list("initialIndex" = initialIndex, "finalIndex" = finalIndex,
                               "cv" = coefVar, "shapiro" = shapiroVal, "ad" = adVal,
                               "normality" = normalityAgreed, "bestDistFit" = bestFitDistr,
                               "runs" = runsVal, "ljungBox" = boxVal, "IQR" = outIQR)
    }
    #partialResult
    
  }
# })

# t3 <- system.time({
stopImplicitCluster()

# Save the results to a CSV file
dfSave2 <- data.frame(matrix(unlist(fullResultLong[,1:600]), nrow=600, byrow=TRUE),stringsAsFactors=FALSE)
colnames(dfSave2) <- c("initialIndex", "finalIndex", "cv", "shapiro", "ad", "normality", "bestDistFit", "runs", "ljungBox", "IQR")
write.table(dfSave2, file="C:/PhD/variationProcessLong.csv", append= T, sep=',')

# Generate a new set of resources for execution
myCluster <- makeCluster(defaultCores, type = "PSOCK")
myCluster
registerDoParallel(cl=myCluster)

# Define the short data chunks with sliding windows of 15 steps
shortWindowLength <- round (numMetrics*0.01, 0)
referenceIndexesShort <- sample (shortWindowLength:numMetrics,20,replace=F)
referenceIndexesShort
# })

# Iterate through the data chunks with short windows
# t4 <- system.time({
  fullResultShort <- foreach (i=1:20, .combine = 'cbind', .packages=c("nortest", "randtests", "foreach", "fitdistrplus", "EnvStats")) %dopar% {
    currentIndex <- referenceIndexesShort[i]
    
    # Prepare the sliding windows (15 steps)
    partialResult <- foreach (j=1:15, .combine = 'cbind') %do% {
      initialIndex <- currentIndex+j-1-shortWindowLength+j-1
      finalIndex <- currentIndex+j-1
      sensorDataChunk <- sensorDataFull [initialIndex:finalIndex] 
      print(paste("Initial Index: ", initialIndex))
      print(paste("Final Index: ", finalIndex))
      print(sensorDataChunk)
      resultSlidingWindow <- variation_analysis(sensorDataChunk, i, absoluteZero)
      
      # Extract results for outcome preparation
      coefVar <- getElement(resultSlidingWindow, "cv")
      shapiroVal <- getElement(resultSlidingWindow, "shapiro")
      adVal <- getElement(resultSlidingWindow, "ad")
      normalityAgreed <- getElement(resultSlidingWindow, "normality")
      bestFitDistr <- getElement(resultSlidingWindow, "bestDistFit")
      runsVal <- getElement(resultSlidingWindow, "runs")
      boxVal <- getElement(resultSlidingWindow, "ljungBox")
      outIQR <- getElement(resultSlidingWindow, "IQR")
      
      # Leave the results in a list, in memory, for combination with the other loops results
      myFullReturnList <- list("initialIndex" = initialIndex, "finalIndex" = finalIndex,
                               "cv" = coefVar, "shapiro" = shapiroVal, "ad" = adVal,
                               "normality" = normalityAgreed, "bestDistFit" = bestFitDistr,
                               "runs" = runsVal, "ljungBox" = boxVal, "IQR" = outIQR)
    }
    #partialResult
    
  }
# })

# t5 <- system.time({
# Stop the resources for parallelization
stopImplicitCluster()

# Save the results to a CSV file
dfSave <- data.frame(matrix(unlist(fullResultShort[,1:300]), nrow=300, byrow=TRUE),stringsAsFactors=FALSE)
colnames(dfSave) <- c("initialIndex", "finalIndex", "cv", "shapiro", "ad", "normality", "bestDistFit", "runs", "ljungBox", "IQR")
write.table(dfSave, file="C:/PhD/variationProcessShort.csv", append= T, sep=',')

# })


# Check normality of the full dataset with Anderson-Darling
normalityTestFull <- ad.test(sensorDataFull)
normalityTestFull

if (normalityTestFull$p.value<0.05) {
  cat("Anderson-Darling reported the dataset is not normal \n")
} else {
  cat("Anderson-Darling reported the dataset is normal \n")
}








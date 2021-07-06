########################################################################################
## Script for automating the process of variation analysis
## -------------------------------------------------------------------------------------
## Copyright (c) 2021 F. Javier Nieto. All rights reserved.
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
inputFile <- 'C:/PhD/bosch_sensor_1.csv'


# Function that looks for the best distribution fitting the current dataset
distribution_fit <- function (sensorData) {
  
  # If there are avalues below 0, we move the dataset up --> Necessary for weibull, gamma...
  minValue <- min(sensorData)
  if (minValue<0) {
    sensorData <- sensorData + ((minValue*-1) +1)
  }
  
  # Calculate fit for 3 distributions: weibull, lognormal, gamma
  fweibull <- fitdist(sensorData, "weibull")
  fgamma <- fitdist(sensorData, "gamma")
  flnorm <- fitdist(sensorData, "lnorm")
  fnorm <- fitdist(sensorData, "norm")
  funif <- fitdist(sensorData, "unif")
  #fburr <- fitdist(sensorData, "burr", start = list(shape1 = 0.3, shape2 = 1, rate = 1)) #issues with 'actuar' library
  
  #cdfcomp(list(fweibull, fgamma, flnorm, fnorm, funif), 
  #        xlogscale = TRUE, ylogscale = TRUE, legendtext = c("weibull", "gamma", "lognormal", "normal", "unif"))
  
  # Compare the goodness of fit for all the distributions 
  gofResult <- gofstat(list(fweibull, fgamma, flnorm, fnorm, funif), fitnames = c("weibull", "gamma", "lnorm", "normal", "unif"))
  aicList <- c(gofResult$aic[[1]], gofResult$aic[[2]], gofResult$aic[[3]], gofResult$aic[[4]], gofResult$aic[[5]])
  bicList <- c(gofResult$bic[[1]], gofResult$bic[[2]], gofResult$bic[[3]], gofResult$bic[[4]], gofResult$bic[[5]])
  ksList <- c(gofResult$ks[[1]], gofResult$ks[[2]], gofResult$ks[[3]],gofResult$ks[[4]], gofResult$ks[[5]])
  cvmList <- c(gofResult$cvm[[1]], gofResult$cvm[[2]], gofResult$cvm[[3]], gofResult$cvm[[4]], gofResult$cvm[[5]])
  adList <- c(gofResult$ad[[1]], gofResult$ad[[2]], gofResult$ad[[3]], gofResult$ad[[4]], gofResult$ad[[5]])
  fullGofList <- list(aicList, bicList, ksList, cvmList, adList)
  
  # Revise the results and indicate the one with best fit
  resGofList <- c(0, 0, 0, 0, 0)
  for (i in 1:5) {
    resGofList[which.min(fullGofList[[i]])] <- resGofList[which.min(fullGofList[[i]])]+1
  }
  bestGofFit <- which.max(resGofList)
  distributionResult <- switch(bestGofFit, '1' = {fweibull}, '2' = {fgamma}, '3' = {flnorm}, '4' = {fnorm}, '5' = {funif})
  return (distributionResult)
}


# Function that calculates all the tests --> So its execution can be parallelized
variation_analysis <- function (sensorData, iteration, abZero) {
  
    # Create a plot with the data 
  title <- paste ("Iteration", iteration)
  plot(sensorData, col="green", main=title)
  
  # Calculate coefficient of variation
  coefVar <- NaN
  if (abZero) {
    coefVar <- cv(sensorData)
  }
  
  # Check normality of the data with Shapiro-Wilk
  normalityTest1 <- shapiro.test(sensorData)
  shapiroValue <- getElement(normalityTest1, "p.value")
  #print(normalityTest1)
  
  # Check normality of the data with Anderson-Darling
  normalityTest2 <- ad.test(sensorData)
  adValue <- getElement(normalityTest2, "p.value")
  #print(normalityTest2)
  
  dataDistribution <- "NORMAL"
  # Normality consensus and calculate mean and variance
  normalityAgreed <- TRUE
  bestFit <- NaN
  bestFitName <- NaN
  if (normalityTest1$p.value<0.05 | normalityTest2$p.value<0.05) {
    #print("The tests reported the dataset is not normal.")
    normalityAgreed <- FALSE
    
    # Calculate best fit of the dataset for 3 distributions: weibull, lognormal, gamma
    bestFit <- distribution_fit(sensorData)
    bestFitName <- getElement(bestFit, "distname")
    
  } else {
    #print("The tests reported the dataset is normal.")
  }
  
  # Ljung-Box test
  outBox = Box.test(sensorData, lag = 1, type = "Ljung")
  #print(outBox)
  
  # Runs test with mean as reference (median can be used)
  myThreshold <- mean(sensorData)
  outRuns = runs.test(sensorData, threshold = myThreshold)
  runsVal <- getElement(outRuns, "p.value")
  #print(outRuns)
  randomData <- TRUE
  if (outRuns$p.value < 0.05) {
    randomData <- FALSE
  }
  
  # Calculate quartiles and IQR
  outQuantile=quantile(sensorData)
  #print(outQuantile)
  outIQR <- IQR(sensorData)
  #print(outIQR)
  
  # Prepare result of the function
  myReturnList <- list ("cv" = coefVar, "shapiro" = shapiroValue, "ad" = adValue, 
                        "normality" = normalityAgreed, "bestDistFit" = bestFitName,
                        "runs" = runsVal, "IQR" = outIQR)
  return(myReturnList)
}

# Main code of the process

# Load the file with the data
fullDataset <- read.csv(inputFile, sep = ",", header = TRUE)

# Extract the data we are interested in and plot the full dataset
sensorDataFull <- fullDataset[,5]
plot(sensorDataFull, col="green", main="Full dataset")

# Define the long data chunks with sliding windows of 50 steps
numMetrics <- length(sensorDataFull)
longWindowLength <- round (numMetrics*0.06, 0)
referenceIndexesLong <- sample (longWindowLength:numMetrics,12,replace=F)
referenceIndexesLong

# Set up the parallel environment PSOCK cluster, so it can be used in Windows and Linux
defaultCores <- detectCores()-1
defaultCores
myCluster <- makeCluster(defaultCores, type = "PSOCK")
myCluster
registerDoParallel(cl=myCluster)

# Iterate through the data chunks with long windows
system.time({
  fullResultLong <- foreach (i=1:12, .combine = 'cbind', .packages=c("nortest", "randtests", "foreach", "fitdistrplus", "EnvStats")) %dopar% {
    currentIndex <- referenceIndexesLong[i]
    
    # Prepare the sliding windows (50 steps)
    partialResult <- foreach (j=1:50, .combine = 'cbind') %do% {
      initialIndex <- currentIndex+j-1-longWindowLength+j-1
      finalIndex <- currentIndex+j-1
      sensorDataChunk <- sensorDataFull [initialIndex:finalIndex] 
      resultSlidingWindow <- variation_analysis(sensorDataChunk, 1, absoluteZero)
      
      # Extract results for outcome preparation
      coefVar <- getElement(resultSlidingWindow, "cv")
      shapiroVal <- getElement(resultSlidingWindow, "shapiro")
      adVal <- getElement(resultSlidingWindow, "ad")
      normalityAgreed <- getElement(resultSlidingWindow, "normality")
      bestFitDistr <- getElement(resultSlidingWindow, "bestDistFit")
      runsVal <- getElement(resultSlidingWindow, "runs")
      outIQR <- getElement(resultSlidingWindow, "IQR")
      
      # Leave the results in a list, in memory, for combination with the other loops results
      myFullReturnList <- list("initialIndex" = initialIndex, "finalIndex" = finalIndex,
                               "cv" = coefVar, "shapiro" = shapiroVal, "ad" = adVal,
                               "normality" = normalityAgreed, "bestDistFit" = bestFitDistr,
                               "runs" = runsVal, "IQR" = outIQR)
    }
    #partialResult
    
  }
})

stopImplicitCluster()

# Save the results to a CSV file
dfSave2 <- data.frame(matrix(unlist(fullResultLong[,1:600]), nrow=600, byrow=TRUE),stringsAsFactors=FALSE)
colnames(dfSave2) <- c("initialIndex", "finalIndex", "cv", "shapiro", "ad", "normality", "bestDistFit", "runs", "IQR")
write.table(dfSave2, file="C:/PhD/variationProcessLong.csv", append= T, sep=',')

# Generate a new set of resources for execution
myCluster <- makeCluster(defaultCores, type = "PSOCK")
myCluster
registerDoParallel(cl=myCluster)

# Define the short data chunks with sliding windows of 15 steps
shortWindowLength <- round (numMetrics*0.025, 0)
referenceIndexesShort <- sample (shortWindowLength:numMetrics,20,replace=F)
referenceIndexesShort

# Iterate through the data chunks with short windows
system.time({
  fullResultShort <- foreach (i=1:20, .combine = 'cbind', .packages=c("nortest", "randtests", "foreach", "fitdistrplus", "EnvStats")) %dopar% {
    currentIndex <- referenceIndexesShort[i]
    
    # Prepare the sliding windows (15 steps)
    partialResult <- foreach (j=1:15, .combine = 'cbind') %do% {
      initialIndex <- currentIndex+j-1-shortWindowLength+j-1
      finalIndex <- currentIndex+j-1
      sensorDataChunk <- sensorDataFull [initialIndex:finalIndex] 
      resultSlidingWindow <- variation_analysis(sensorDataChunk, i, absoluteZero)
      
      # Extract results for outcome preparation
      coefVar <- getElement(resultSlidingWindow, "cv")
      shapiroVal <- getElement(resultSlidingWindow, "shapiro")
      adVal <- getElement(resultSlidingWindow, "ad")
      normalityAgreed <- getElement(resultSlidingWindow, "normality")
      bestFitDistr <- getElement(resultSlidingWindow, "bestDistFit")
      runsVal <- getElement(resultSlidingWindow, "runs")
      outIQR <- getElement(resultSlidingWindow, "IQR")
      
      # Leave the results in a list, in memory, for combination with the other loops results
      myFullReturnList <- list("initialIndex" = initialIndex, "finalIndex" = finalIndex,
                               "cv" = coefVar, "shapiro" = shapiroVal, "ad" = adVal,
                               "normality" = normalityAgreed, "bestDistFit" = bestFitDistr,
                               "runs" = runsVal, "IQR" = outIQR)
    }
    #partialResult
    
  }
})

# Stop the resources for parallelization
stopImplicitCluster()

# Save the results to a CSV file
dfSave <- data.frame(matrix(unlist(fullResultShort[,1:300]), nrow=300, byrow=TRUE),stringsAsFactors=FALSE)
colnames(dfSave) <- c("initialIndex", "finalIndex", "cv", "shapiro", "ad", "normality", "bestDistFit", "runs", "IQR")
write.table(dfSave, file="C:/PhD/variationProcessShort.csv", append= T, sep=',')


# Check normality of the full dataset with Anderson-Darling
normalityTestFull <- ad.test(sensorDataFull)
normalityTestFull

if (normalityTestFull$p.value<0.05) {
  cat("Anderson-Darling reported the dataset is not normal \n")
} else {
  cat("Anderson-Darling reported the dataset is normal \n")
}








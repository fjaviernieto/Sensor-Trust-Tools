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

library(EnvStats)
library(nortest)
library(trend)
library(outliers)
library(foreach)
library(doParallel)

############################
# Configuration parameters #
############################

inputFile <- 'C:/PhD/Arduino/station_arduino.csv'

# Function that calculates all the tests --> So its execution can be parallelized
outliers_analysis <- function (sensorData) {
  
  # Remove NA values (or SNHT fails) --> Consider as outlier 
  sensorData[is.na(sensorData)] <- -99.99
  #plot(sensorData, col="green")
  
  # Check whether there is strong trend in the data or not
  outMk = mk.test(sensorData)
  print(outMk)
  mkStatistic <- outMk$estimates[[1]]
  mkValue <- outMk$p.value
  isTrend <- FALSE
  dataTrend <- "none"
  
  if (mkValue < 0.05) {
    # We identify the type of trend
    isTrend <- TRUE
    dataTrend <- "increase"
    if (mkStatistic < 0) {
      dataTrend <- "decrease"
    }
  }
  
  if (isTrend) {
    # Remove the trend from the data with diffs level 1 (linear)
    sensorData <- diff(sensorData, differences = 1)
    sensorData[is.nan(sensorData)] <- -99.99
  }
  
  print(sensorData)
  
  # Check normality of the data with Shapiro-Wilk
  normalityTest1 <- shapiro.test(sensorData)
  print(normalityTest1)
  
  # Check normality of the data with Anderson-Darling
  normalityTest2 <- ad.test(sensorData)
  print(normalityTest2)
  
  dataDistribution <- "NORMAL"
  # Normality consensus and calculate mean and variance
  normalityAgreed <- TRUE
  buishandRangeVal <- NaN
  buishandUVal <- NaN
  if (normalityTest1$p.value<0.05 | normalityTest2$p.value<0.05) {
    print("The tests reported the dataset is not normal.")
    normalityAgreed <- FALSE
  } else {
    #print("The tests reported the dataset is normal.")
    
    # Calculate Buishand tests
    resBr <- br.test(sensorData, m=3000)
    print(resBr)
    buishandRangeVal <- getElement(resBr, "p.value")
    
    resBu <- bu.test(sensorData, m=3000)
    print(resBr)
    buishandUVal <- getElement(resBu, "p.value")
  }
  
  # Calculate SNHT
  resSNHT <- snh.test(sensorData, 3000)
  print(resSNHT)
  SNHTVal <- getElement(resSNHT, "p.value")
  
  # Calculate Pettitt
  resPettitt <- pettitt.test(sensorData)
  print(resPettitt)
  pettittVal <- getElement(resPettitt, "p.value")
  
  # Calculate Lanzante
  resLanzante <- lanzante.test(sensorData)
  print(resLanzante)
  lanzanteVal <- getElement(resLanzante, "p.value")
  
  # Calculate Grubbs
  resGrubbs <- grubbs.test(sensorData)
  print(resGrubbs)
  grubbsVal <- getElement(resGrubbs, "p.value")
  
  # Calculate Rosner's ESD
  #resESD <- rosnerTest(sensorData, k=7)
  #print(resESD)
  #ESDVal <- resESD$n.outliers #It fails when no outliers are detected! --> TBD
  
  # Check if any of them detected outliers
  detectedOutliers <- FALSE
  if (SNHTVal<0.05 | grubbsVal<0.05 | pettittVal<0.05 | lanzanteVal<0.05) {
    detectedOutliers <- TRUE
  }
  
  # Prepare result of the function
  myReturnList <- list ("shapiro" = normalityTest1$p.value, "ad" = normalityTest2$p.value, 
                        "normality" = normalityAgreed, "trend" = dataTrend, "outliers" = detectedOutliers,
                        "snht" = SNHTVal, "grubbs" = grubbsVal, "pettitt" = pettittVal, "lanzante" = lanzanteVal, 
                        "buishandU" = buishandUVal, "buishandRange" = buishandRangeVal)
  return(myReturnList)
}

# Main code of the process

# Load the file with the data
fullDataset <- read.csv(inputFile, sep = ",", header = TRUE)

# Extract the data we are interested in and plot the full dataset
sensorDataFull <- fullDataset[,3]
sensorDataFull[is.na(sensorDataFull)] <- -99.99
plot(sensorDataFull, col="green", main="Full dataset")
t1 <- system.time({
# Define the long data chunks with sliding windows of 50 steps
numMetrics <- length(sensorDataFull)
longWindowLength <- round (numMetrics*0.06, 0)
referenceIndexesLong <- sample (longWindowLength:numMetrics,12,replace=F)
referenceIndexesLong

# Set up the parallel environment PSOCK cluster, so it can be used in Windows and Linux
#defaultCores <- detectCores()-1
defaultCores <- 4
defaultCores
myCluster <- makeCluster(defaultCores, type = "PSOCK")
myCluster
registerDoParallel(cl=myCluster)
})

# Iterate through the data chunks with long windows
t2 <- system.time({
  fullResultLong <- foreach (i=1:10, .combine = 'cbind', .packages=c("nortest", "foreach", "trend", "outliers", "EnvStats")) %dopar% {
    currentIndex <- referenceIndexesLong[i]
    
    # Prepare the sliding windows (30 steps)
    partialResult <- foreach (j=1:30, .combine = 'cbind') %do% {
      initialIndex <- currentIndex+j-1-longWindowLength+j-1
      finalIndex <- currentIndex+j-1
      sensorDataChunk <- sensorDataFull [initialIndex:finalIndex] 
      resultSlidingWindow <- outliers_analysis(sensorDataChunk)
      
      # Extract results for outcome preparation
      shapiroVal <- getElement(resultSlidingWindow, "shapiro")
      adVal <- getElement(resultSlidingWindow, "ad")
      normalityAgreed <- getElement(resultSlidingWindow, "normality")
      dataTrend <- getElement(resultSlidingWindow, "trend")
      detectedOutliers <- getElement(resultSlidingWindow, "outliers")
      SNHTVal <- getElement(resultSlidingWindow, "snht")
      grubbsVal <- getElement(resultSlidingWindow, "grubbs")
      pettittVal <- getElement(resultSlidingWindow, "pettitt")
      lanzanteVal <- getElement(resultSlidingWindow, "lanzante")
      buishandUVal <- getElement(resultSlidingWindow, "buishandU")
      buishandRangeVal <- getElement(resultSlidingWindow, "buishandRange")
      #ESDVal <- getElement(resultSlidingWindow, "ESD")
      
      # Leave the results in a list, in memory, for combination with the other loops results
      myFullReturnList <- list("initialIndex" = initialIndex, "finalIndex" = finalIndex, 
                               "shapiro" = shapiroVal, "ad" = adVal, "normality" = normalityAgreed, 
                               "trend" = dataTrend, "outliers" = detectedOutliers, "snht" = SNHTVal, 
                               "grubbs" = grubbsVal, "pettitt" = pettittVal, "lanzante" = lanzanteVal, 
                               "buishandU" = buishandUVal, "buishandRange" = buishandRangeVal)
    }
    #partialResult
    
  }
})

t3 <- system.time({
stopImplicitCluster()

# Save the results to a CSV file
dfSave <- data.frame(matrix(unlist(fullResultLong[,1:300]), nrow=300, byrow=TRUE),stringsAsFactors=FALSE)
colnames(dfSave) <- c("initialIndex", "finalIndex", "shapiro", "ad", "normality", "trend", "outliers", 
                      "snht", "grubbs", "pettitt", "lanzante", "buishandU", "buishandRange")
write.table(dfSave, file="C:/PhD/outliersProcessLong.csv", append= T, sep=',')

# Generate a new set of resources for execution
myCluster <- makeCluster(defaultCores, type = "PSOCK")
myCluster
registerDoParallel(cl=myCluster)

# Define the short data chunks with sliding windows of 15 steps
shortWindowLength <- round (numMetrics*0.01, 0)
referenceIndexesShort <- sample (shortWindowLength:numMetrics,15,replace=F)
referenceIndexesShort
})

# Iterate through the data chunks with short windows
t4 <- system.time({
  fullResultShort <- foreach (i=1:15, .combine = 'cbind', .packages=c("nortest", "foreach", "trend", "outliers", "EnvStats")) %dopar% {
    currentIndex <- referenceIndexesShort[i]
    
    # Prepare the sliding windows (15 steps)
    partialResult <- foreach (j=1:15, .combine = 'cbind') %do% {
      initialIndex <- currentIndex+j-1-shortWindowLength+j-1
      finalIndex <- currentIndex+j-1
      sensorDataChunk <- sensorDataFull [initialIndex:finalIndex] 
      resultSlidingWindow <- outliers_analysis(sensorDataChunk)
      
      # Extract results for outcome preparation
      shapiroVal <- getElement(resultSlidingWindow, "shapiro")
      adVal <- getElement(resultSlidingWindow, "ad")
      normalityAgreed <- getElement(resultSlidingWindow, "normality")
      dataTrend <- getElement(resultSlidingWindow, "trend")
      detectedOutliers <- getElement(resultSlidingWindow, "outliers")
      SNHTVal <- getElement(resultSlidingWindow, "snht")
      grubbsVal <- getElement(resultSlidingWindow, "grubbs")
      pettittVal <- getElement(resultSlidingWindow, "pettitt")
      lanzanteVal <- getElement(resultSlidingWindow, "lanzante")
      buishandUVal <- getElement(resultSlidingWindow, "buishandU")
      buishandRangeVal <- getElement(resultSlidingWindow, "buishandRange")
      #ESDVal <- getElement(resultSlidingWindow, "ESD")
      
      # Leave the results in a list, in memory, for combination with the other loops results
      myFullReturnList <- list("initialIndex" = initialIndex, "finalIndex" = finalIndex, 
                               "shapiro" = shapiroVal, "ad" = adVal, "normality" = normalityAgreed, 
                               "trend" = dataTrend, "outliers" = detectedOutliers, "snht" = SNHTVal, 
                               "grubbs" = grubbsVal, "pettitt" = pettittVal, "lanzante" = lanzanteVal, 
                               "buishandU" = buishandUVal, "buishandRange" = buishandRangeVal)
    }
    #partialResult
    
  }
})

t5 <- system.time({
stopImplicitCluster()

# Save the results to a CSV file
dfSave2 <- data.frame(matrix(unlist(fullResultShort[,1:225]), nrow=225, byrow=TRUE),stringsAsFactors=FALSE)
colnames(dfSave2) <- c("initialIndex", "finalIndex", "shapiro", "ad", "normality", "trend", "outliers", 
                      "snht", "grubbs", "pettitt", "lanzante", "buishandU", "buishandRange")
write.table(dfSave2, file="C:/PhD/outliersProcessShort.csv", append= T, sep=',')
})
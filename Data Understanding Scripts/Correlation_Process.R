########################################################################################
## Script for automating the process of correlation analysis
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

library(nortest)
library(foreach)
library(doParallel)

############################
# Configuration parameters #
############################

inputFile1 <- 'C:/PhD/bosch_sensor_1.csv'
inputFile2 <- 'C:/PhD/bosch_sensor_2.csv'
outliersExpected <- FALSE

# Function that calculates all the tests --> So its execution can be parallelized
correlation_analysis <- function (sensorData1, sensorData2, outliers) {
  
  # Check normality of the data with Shapiro-Wilk
  normalityTest1 <- shapiro.test(sensorData1)
  print(normalityTest1)
  
  # Check normality of the data with Anderson-Darling
  normalityTest2 <- ad.test(sensorData1)
  print(normalityTest2)
  
  dataDistribution <- "NORMAL"
  # Normality consensus and calculate mean and variance
  normalityAgreed <- TRUE
  outPearson <- NaN
  estPearson <- NaN
  outKendall <- NaN
  outSpearman <- NaN
  
  if (normalityTest1$p.value<0.05 | normalityTest2$p.value<0.05) {
    print("The tests reported the dataset is not normal.")
    normalityAgreed <- FALSE
  } else if (!outliers) {
    print("The tests reported the dataset is normal. Calculate Pearson")
    outPearson <- cor.test(sensorData1, sensorData2, method="pearson")
    estPearson <- getElement(outPearson, "estimate")[[1]]
    print(outPearson)
  }
  
  # Correlation with Kendall
  outKendall <- cor.test(sensorData1, sensorData2, method="kendall")
  estKendall <- getElement(outKendall, "estimate")[[1]]
  print(outKendall)
  
  # Correlation with Spearman
  outSpearman <- cor.test(sensorData1, sensorData2, method="spearman")
  estSpearman <- getElement(outSpearman, "estimate")[[1]]
  print(outSpearman)
  
  # Check if the tests reported correlation
  haveCorrelation <- FALSE
  corrText <- "NoCorrelated"
  if (estKendall >= 0.5 | estSpearman >= 0.5) {
    print("There is correlation between the datasets!")
    haveCorrelation <- TRUE
    corrText <- "YesCorrelated"
  #} else if (!is.nan(outPearson) & outPearson$estimate >= 0.5) {
  #  print("There is correlation between the datasets!")
  #  haveCorrelation <- TRUE
  }
  
  # Prepare result of the function
  myReturnList <- list ("shapiro" = normalityTest1$p.value, "ad" = normalityTest2$p.value, 
                        "normality" = normalityAgreed, "correlation" = haveCorrelation, "cortext" = corrText,
                        "kendall" = estKendall, "spearman" = estSpearman, "pearson" = estPearson)
  return(myReturnList)
}


# Main code of the process

# Load the files with the data
fullDataset1 <- read.csv(inputFile1, sep = ",", header = TRUE)
fullDataset2 <- read.csv(inputFile2, sep = ",", header = TRUE)

# Extract the data we are interested in and plot the full datasets
sensorDataFull1 <- fullDataset1[,5]
plot(sensorDataFull1, col="green", main="Full dataset")
# Extract the data we are interested in and plot the full datasets
sensorDataFull2 <- fullDataset2[,5]
plot(sensorDataFull2, col="green", main="Full dataset")

# See full correlation
resultDataFull <- correlation_analysis(sensorDataFull1[1:4999], sensorDataFull2[1:4999], outliersExpected)

# Define the long data chunks with sliding windows of 50 steps
numMetrics <- length(sensorDataFull1)
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
  fullResult <- foreach (i=1:12, .combine = 'cbind', .packages=c("nortest", "foreach")) %do% {
    currentIndex <- referenceIndexesLong[i]
    
    # Prepare the sliding windows (50 steps)
    partialResult <- foreach (j=1:50, .combine = 'cbind') %do% {
      initialIndex <- currentIndex+j-1-longWindowLength+j-1
      finalIndex <- currentIndex+j-1
      sensorDataChunk1 <- sensorDataFull1 [initialIndex:finalIndex] 
      sensorDataChunk2 <- sensorDataFull2 [initialIndex:finalIndex]
      resultSlidingWindow <- correlation_analysis(sensorDataChunk1, sensorDataChunk2, outliersExpected)
      
      # Extract results for outcome preparation
      shapiroVal <- getElement(resultSlidingWindow, "shapiro")
      adVal <- getElement(resultSlidingWindow, "ad")
      normalityAgreed <- getElement(resultSlidingWindow, "normality")
      haveCorrelation <- getElement(resultSlidingWindow, "correlation")
      corrText <- getElement(resultSlidingWindow, "cortext")
      estKendall <- getElement(resultSlidingWindow, "kendall")
      estSpearman <- getElement(resultSlidingWindow, "spearman")
      estPearson <- getElement(resultSlidingWindow, "pearson")
      
      # Leave the results in a list, in memory, for combination with the other loops results
      myFullReturnList <- list("initialIndex" = initialIndex, "finalIndex" = finalIndex, 
                               "shapiro" = shapiroVal, "ad" = adVal, 
                               "normality" = normalityAgreed, "correlation" = haveCorrelation, "cortext" = corrText,
                               "kendall" = estKendall, "spearman" = estSpearman, "pearson" = estPearson)
    }
    
    #partialResult
    
  }
})

# Save the results to a CSV file
dfSave <- data.frame(matrix(unlist(fullResult[,1:600]), nrow=600, byrow=TRUE),stringsAsFactors=FALSE)
colnames(dfSave) <- c("initialIndex", "finalIndex", "shapiro", "ad", "normality", 
                      "correlation", "cortext", "kendall", "spearman", "pearson")
write.table(dfSave, file="C:/PhD/correlationProcess.csv", append= T, sep=',')

stopImplicitCluster()


########################################################################################
## Script for integrating the outcomes from the variation process and outliers detection
## to obtain a complete vision of the dataset analysis. Such information can be, later
## on, combined in order to obtain an estimation of trustworthiness for a sensor.
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

library(EnvStats)
library(nortest)
library(trend)
library(outliers)
library(randtests)
library(foreach)
library(doParallel)
library(ggpmisc)
library(MLmetrics)

############################
# Configuration parameters #
############################

inputFile <- 'C:/PhD/Arduino/station_arduino.csv'
inputFile <- 'C:/PhD/FaultInjectionDatasets/sensorscope/interpolated/injected_bias/mote=\'15\'_sensortype=temperature_faulttype=bias.txt'
inputFile <- 'C:/PhD/FaultInjectionDatasets/sensorscope/interpolated/injected_drift/mote=\'15\'_sensortype=temperature_faulttype=drift.txt'
inputFile <- 'C:/PhD/FaultInjectionDatasets/sensorscope/interpolated/injected_malfunction/mote=\'15\'_sensortype=temperature_faulttype=malfunction.txt'
inputFile <- 'C:/PhD/FaultInjectionDatasets/sensorscope/interpolated/injected_random/mote=\'15\'_sensortype=temperature_faulttype=random.txt'

inputFile <- 'C:/PhD/HiDALGO_Bosch/1WeekAirQualityBosch_annotated_final.csv'

# Function to do exponential difference transformation
exp_diff <- function(inputData){
  transform_result <- rep (0, (length(inputData)-1))
  for (i in 2:length(inputData)){
    input_diff <- inputData[i] - inputData[i-1]
    # transform_result[i] <- abs(input_diff) * 1.2^(abs(input_diff) - 15)
    transform_result[i] <- input_diff * 1.1^(abs(input_diff) - 15)
  }

  return(transform_result)
}

# Function to do polynomial regression transformation
poly_reg <- function(inputData){
  x <- 1:(length(inputData))
  predictionModel <- lm(inputData~poly(x,5,raw=TRUE))
  prediction <- predict(predictionModel,newdata=data.frame(x))
  transform_result <- inputData - prediction
  return(transform_result)
}

# Function that calculates all the angles in local maximum/minimum points
turning_points_analysis <- function (testData) {
  # First of all, if all values are equal, the tests fail -> Return angle 0
  if (length(unique(testData))==1){
    print("Unique Value!")
    myReturnList <- list ("numMax" = 0, "numMin" = 0, "maxPositions" = 0,
                          "minPositions" = 0, "angles" = list(180))
    return(myReturnList)
  }

  # Prepare the data in a dataframe
  x <- seq(1,length(testData))
  dataShow <- data.frame(
    x = x,
    y = testData
  )

  # Plot local maximum and minimum
  ggplot(data = dataShow, aes(x=x, y=y)) + geom_line() + stat_peaks(span=5, strict=TRUE, col = "red") +
    stat_valleys(span=5, strict=TRUE, col = "blue")

  # Extract max and min from the data and their position in the time series
  maximos <- ggpmisc:::find_peaks(dataShow$y, span=5)
  pos_max <- which(maximos==TRUE)
  minimos <- ggpmisc:::find_peaks(-dataShow$y, span=5)
  pos_min <- which(minimos==TRUE)
  positions <- c(pos_max, pos_min)
  angles <- list()
  angles_conv <- list()
  num_max <- length(pos_max)
  num_min <- length(pos_min)
  # fullResultList <- rep(list(), 7)
  index <- 1

  # Prepare limits for the plots
  limMin <- min(dataShow$y) + (-20)
  limMax <- max(dataShow$y) + 20

  # Calculate the slope taking as reference max/min and points nearby to create two lines
  for (i in positions){
    xref <- i
    yref <- dataShow$y[i]
    x1 <- i - 1
    y1 <- dataShow$y[(i-1)]
    x2 <- i + 1
    y2 <- dataShow$y[(i+1)]

    # Equations of the lines generated
    m1 <- (yref - y1) / (xref - x1)
    n1 <- y1 - (((yref - y1)*x1)/(xref-x1))
    m2 <- (y2 - yref) / (x2 - xref)
    n2 <- yref - (((y2 - yref)*xref)/(x2-xref))

    # Calculate angle
    # tanResult <- abs((m2 - m1) / (1 + m1 * m2))
    # tanResult <- (m2 - m1) / (1 + m1 * m2)
    # angle <- atan(tanResult) * (180/pi)
    tanInput1 <- m2-m1
    tanInput2 <- 1 + m1 * m2
    angle <- atan2(tanInput1, tanInput2) * (180/pi)

    # Plot the lines generated with the statistic
    plot_limit = c(0, limMax)
    if(yref<0){
      plot_limit = c(limMin, 0)
    }
    #plot(x,(m1*x+n1),col='red', type='l', ylim = plot_limit)
    #lines(x,(m2*x+n2),col='green')
    #lines(testData, col='blue', type='p')

    # Add angle to the list
    angles[index] <- angle
    index <- index + 1
  }

  # Transform the angles, taking into account if they come from local max or min
  if(num_max > 0) {
    angles_conv[1:num_max] <- (-180 - as.numeric(unlist(angles[1:num_max]))) * -1
  }

  if(num_min > 0) {
    angles_conv[1+num_max:length(positions)] <- 180 - as.numeric(unlist(angles[1+num_max:length(positions)]))
  }

  # Prepare result of the function
  myReturnList <- list ("numMax" = length(pos_max), "numMin" = length(pos_min), "maxPositions" = pos_max,
                        "minPositions" = pos_min, "angles" = angles_conv)
  return(myReturnList)
}

# Function that calculates all the outliers tests --> So its execution can be parallelized
outliers_analysis <- function (sensorData) {
  # First of all, if all values are equal, the tests fail -> Return error and all 0
  if (length(unique(sensorData))==1){
    print("Unique Value!")
    myReturnList <- list ("shapiro" = 1, "ad" = 1,
                          "normality" = FALSE, "trend" = "none", "outliers" = TRUE,
                          "snht" = 0, "grubbs" = 0, "pettitt" = 0, "lanzante" = 0,
                          "buishandU" = 0, "buishandRange" = 0, "grubbsEDT" = 0,
                          "snhtEDT" = 0, "pettittEDT" = 0, "lanzanteEDT" = 0,
                          "buishandREDT" = 0, "buishandUEDT" = 0,
                          "angleSNHT" = 1, "anglePettitt" = 1, "angleLanzante" = 1,
                          "angleBR" = 1, "angleBU" = 1)
    return(myReturnList)
  }

  # Remove NA and NAN values (or SNHT fails) --> Consider as outlier
  sensorData[is.na(sensorData)] <- -99.99
  sensorData[is.nan(sensorData)] <- -99.99
  #plot(sensorData, col="green")

  # Transform the data, ready to be used
  sensorDataEDT <- exp_diff(sensorData)
  sensorDataPRT <- poly_reg(sensorData)

  # Check whether there is strong trend in the data or not
  outMk = mk.test(sensorData)
  #print(outMk)
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

  # Check normality of the data with Shapiro-Wilk
  normalityTest1 <- shapiro.test(sensorData)
  #print(normalityTest1)

  # Check normality of the data with Anderson-Darling
  normalityTest2 <- ad.test(sensorData)
  #print(normalityTest2)

  dataDistribution <- "NORMAL"
  # Normality consensus and calculate mean and variance
  normalityAgreed <- TRUE
  buishandRangeVal <- NaN
  buishandRangeValEDT <- NaN
  buishandUVal <- NaN
  buishandUValEDT <- NaN
  resAngleBR <- 0
  resAngleBU <- 0
  if (normalityTest1$p.value<0.05 | normalityTest2$p.value<0.05) {
    print("The tests reported the dataset is not normal.")
    normalityAgreed <- FALSE
  } else {
    #print("The tests reported the dataset is normal.")

    # Calculate Buishand tests
    # Calculate Buishand R with original data (necessary for angles analysis)
    resBr <- br.test(sensorData, m=3000)
    #print(resBr)
    # Calculate Buishand R with ED Transformation
    resBrEDT <- br.test(sensorDataEDT, m=3000)
    # Check angles
    brAnglesResult <- turning_points_analysis(resBr$data)
    if (any(88>=brAnglesResult$angles)){
      resAngleBR <- 1
    }
    # If there is trend, use Buishand R with PR transformation
    if (isTrend){
      resBr <- br.test(sensorDataPRT, m=3000)
    }
    buishandRangeVal <- getElement(resBr, "p.value")

    # Calculate Buishand U with original data (necessary for angles analysis)
    resBu <- bu.test(sensorData, m=3000)
    #print(resBu)
    # Calculate Buishand U with ED Transformation
    resBuEDT <- bu.test(sensorDataEDT, m=3000)
    # Check angles
    buAnglesResult <- turning_points_analysis(resBu$data)
    if (any(88>=buAnglesResult$angles)){
      resAngleBU <- 1
    }
    # If there is trend, use Buishand R with PR transformation
    if (isTrend){
      resBu <- bu.test(sensorDataPRT, m=3000)
    }
    buishandUVal <- getElement(resBu, "p.value")
  }

  # Calculate SNHT and apply angles check
  # Calculate SNHT with original data (necessary for angles analysis)
  resSNHT <- snh.test(sensorData, 3000)
  #print(resSNHT)
  # Calculate SNHT with ED Transformation
  resSNHTEDT <- snh.test(sensorDataEDT, 3000)
  #print(resSNHTEDT)
  # Check angles
  resAngleSNHT <- 0
  snhtAnglesResult <- turning_points_analysis(resSNHT$data)
  if (any(45>=snhtAnglesResult$angles)){
    resAngleSNHT <- 1
  }
  # If there is trend, use SNHT with PR transformation
  if (isTrend){
    resSNHT <- snh.test(sensorDataPRT, 3000)
  }
  SNHTVal <- getElement(resSNHT, "p.value")
  SNHTValEDT <- getElement(resSNHTEDT, "p.value")

  # Calculate Pettitt
  # Calculate Pettitt with original data (necessary for angles analysis)
  resPettitt <- pettitt.test(sensorData)
  #print(resPettitt)
  # Calculate Pettitt with ED Transformation
  resPettittEDT <- pettitt.test(sensorDataEDT)
  # Check angles
  resAnglePettitt <- 0
  pettittAnglesResult <- turning_points_analysis(resPettitt$data)
  if (any(1.3>=pettittAnglesResult$angles)){
    resAnglePettitt <- 1
  }
  # If there is trend, use Pettitt with PR transformation
  if (isTrend){
    resPettitt <- pettitt.test(sensorDataPRT)
  }
  pettittVal <- getElement(resPettitt, "p.value")
  pettittValEDT <- getElement(resPettittEDT, "p.value")

  # Calculate Lanzante
  # Calculate Lanzante with original data (necessary for angles analysis)
  resLanzante <- lanzante.test(sensorData)
  #print(resLanzante)
  # Calculate Lanzante with ED Transformation
  resLanzanteEDT <- lanzante.test(sensorDataEDT)
  # Check angles
  resAngleLanzante <- 0
  lanzanteAnglesResult <- turning_points_analysis(resLanzante$data)
  if (any(1.3>=lanzanteAnglesResult$angles)){
    resAngleLanzante <- 1
  }
  # If there is trend, use Lanzante with PR transformation
  if (isTrend){
    resLanzante <- lanzante.test(sensorDataPRT)
  }
  lanzanteVal <- getElement(resLanzante, "p.value")
  lanzanteValEDT <- getElement(resLanzanteEDT, "p.value")

  # Calculate Grubbs
  resGrubbs <- grubbs.test(sensorData)
  resGrubbsEDT <- grubbs.test(sensorDataEDT)
  #print(resGrubbs)
  grubbsVal <- getElement(resGrubbs, "p.value")
  grubbsValEDT <- getElement(resGrubbsEDT, "p.value")

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
                        "buishandU" = buishandUVal, "buishandRange" = buishandRangeVal, "grubbsEDT" = grubbsValEDT,
                        "snhtEDT" = SNHTValEDT, "pettittEDT" = pettittValEDT, "lanzanteEDT" = lanzanteValEDT,
                        "buishandREDT" = buishandRangeValEDT, "buishandUEDT" = buishandUValEDT,
                        "angleSNHT" = resAngleSNHT, "anglePettitt" = resAnglePettitt, "angleLanzante" = resAngleLanzante,
                        "angleBR" = resAngleBR, "angleBU" = resAngleBU)
  return(myReturnList)
}

# Function that calculates all the variation tests --> So its execution can be parallelized
variation_analysis <- function (sensorData) {
  # First of all, if all values are equal, the tests fail -> Return error and all 0
  if (length(unique(sensorData))==1){
    print("Unique Value!")
    myReturnList <- list ("cv" = 0, "runs" = 0, "numRuns" = 0, "ljungBox" = 0, "IQR" = 0)
    return(myReturnList)
  }

  # Check if we have random data
  # Runs test with mean as reference (median can be used)
  myThreshold <- mean(sensorData)
  outRuns = runs.test(sensorData, threshold = myThreshold)
  runsVal <- getElement(outRuns, "p.value")
  numRunsVal <- getElement(outRuns, "runs")

  # Ljung-Box test (autocorrelation test that gives idea about white noise time-series)
  outBox = Box.test(sensorData, lag = 1, type = "Ljung")
  boxVal <- getElement(outBox, "p.value")

  # Calculate quartiles and IQR
  # outQuantile=quantile(sensorData)
  outIQR <- IQR(sensorData)

  # Calculate coefficient of variation, transforming the dataset if negative values are present
  coefVar <- -100
  minValue <- min(sensorData)
  if (minValue<0) {
    sensorData <- sensorData + abs(minValue)
  }
  coefVar <- cv(sensorData)

  # Prepare result of the function
  myReturnList <- list ("cv" = coefVar, "runs" = runsVal, "numRuns" = numRunsVal,
                        "ljungBox" = boxVal, "IQR" = outIQR)
  return(myReturnList)
}

# Main code of the process

# Load the file with the data
fullDataset <- read.csv(inputFile, sep = ";", header = TRUE)

# Extract the data we are interested in and plot the full dataset - Injection datasets
sensorDataFull <- fullDataset[,4]
failureData <- fullDataset[,3]
sensorDataFull[is.na(sensorDataFull)] <- -99.99
plot(sensorDataFull, col="green", main="Full dataset")

# Extract the data we are interested in and plot the full dataset - Gyor
fullSensorData <- fullDataset[fullDataset$deviceID==359072065634004,]
sensorDataFull <- fullSensorData[,4]
failureData <- fullSensorData[,11]
plot(sensorDataFull, col="chartreuse3")

t1 <- system.time({
# Define the long data chunks with sliding windows of 50 steps
numMetrics <- length(sensorDataFull)
longWindowLength <- round (numMetrics*0.06, 0)
referenceIndexesLong <- sample (longWindowLength:numMetrics,12,replace=F)
referenceIndexesLong

# Set up the parallel environment PSOCK cluster, so it can be used in Windows and Linux
#defaultCores <- detectCores()-1
defaultCores <- 6
defaultCores
myCluster <- makeCluster(defaultCores, type = "PSOCK")
myCluster
registerDoParallel(cl=myCluster)
})

# Iterate through the data chunks with long windows
t2 <- system.time({
  fullResultLong <- foreach (i=1:10, .combine = 'cbind', .packages=c("nortest", "foreach", "trend", "outliers",
                                                                     "EnvStats", "randtests", "ggpmisc")) %dopar% {
    currentIndex <- referenceIndexesLong[i]

    # Prepare the sliding windows (30 steps)
    partialResult <- foreach (j=1:30, .combine = 'cbind') %do% {
      initialIndex <- currentIndex+j-1-longWindowLength+j-1
      finalIndex <- currentIndex+j-1
      sensorDataChunk <- sensorDataFull [initialIndex:finalIndex]
      resultSlidingWindow <- outliers_analysis(sensorDataChunk)
      resultVarSlidingWindow <- variation_analysis(sensorDataChunk)

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
      grubbsValEDT <- getElement(resultSlidingWindow, "grubbsEDT")
      snhtValEDT <- getElement(resultSlidingWindow, "snhtEDT")
      pettittValEDT <- getElement(resultSlidingWindow, "pettittEDT")
      lanzanteValEDT <- getElement(resultSlidingWindow, "lanzanteEDT")
      buishandRValEDT <- getElement(resultSlidingWindow, "buishandREDT")
      buishandUValEDT <- getElement(resultSlidingWindow, "buishandUEDT")
      resAngleSNHT <- getElement(resultSlidingWindow, "angleSNHT")
      resAnglePettitt <- getElement(resultSlidingWindow, "anglePettitt")
      resAngleLanzante <- getElement(resultSlidingWindow, "angleLanzante")
      resAngleBR <- getElement(resultSlidingWindow, "angleBR")
      resAngleBU <- getElement(resultSlidingWindow, "angleBU")
      resCV <- getElement(resultVarSlidingWindow, "cv")
      resRunsVal <- getElement(resultVarSlidingWindow, "runs")
      resNumRuns <- getElement(resultVarSlidingWindow, "numRuns")
      resLjung <- getElement(resultVarSlidingWindow, "ljungBox")
      resIQR <- getElement(resultVarSlidingWindow, "IQR")

      # Check whether this sample contains error
      hasOriginalFailure <- 0
      if (any(1<=failureData[initialIndex:finalIndex])){
        hasOriginalFailure <- 1
      }

      # Leave the results in a list, in memory, for combination with the other loops results
      myFullReturnList <- list("initialIndex" = initialIndex, "finalIndex" = finalIndex,
                               "shapiro" = shapiroVal, "ad" = adVal, "normality" = normalityAgreed,
                               "trend" = dataTrend, "outliers" = detectedOutliers, "snht" = SNHTVal,
                               "grubbs" = grubbsVal, "pettitt" = pettittVal, "lanzante" = lanzanteVal,
                               "buishandU" = buishandUVal, "buishandRange" = buishandRangeVal,
                               "grubbsEDT" = grubbsValEDT, "snhtEDT" = snhtValEDT, "pettittEDT" = pettittValEDT,
                               "lanzanteEDT" = lanzanteValEDT, "buishandREDT" = buishandRValEDT,
                               "buishandUEDT" = buishandUValEDT, "angleSNHT" = resAngleSNHT,
                               "anglePettitt" = resAnglePettitt, "angleLanzante" = resAngleLanzante,
                               "angleBR" = resAngleBR, "angleBU" = resAngleBU, "originalFailure" = hasOriginalFailure,
                               "cv" = resCV, "runs" = resRunsVal, "numRuns" = resNumRuns, "ljungBox" = resLjung,
                               "IQR" = resIQR)
    }
    #partialResult

  }
})

t3 <- system.time({
stopCluster(myCluster)

# Save the results to a CSV file
dfSave <- data.frame(matrix(unlist(fullResultLong[,1:300]), nrow=300, byrow=TRUE),stringsAsFactors=FALSE)
colnames(dfSave) <- c("initialIndex", "finalIndex", "shapiro", "ad", "normality", "trend", "outliers",
                      "snht", "grubbs", "pettitt", "lanzante", "buishandU", "buishandRange", "grubbsEDT",
                      "snhtEDT", "pettittEDT", "lanzanteEDT", "buishandREDT", "buishandUEDT", "angleSNHT",
                      "anglePettitt", "angleLanzante", "angleBR", "angleBU", "originalFailure", "cv", "runs",
                      "numRuns", "ljungBox", "IQR")
write.table(dfSave, file="C:/PhD/integratedProcessLong.csv", append= T, sep=',', row.names = FALSE)

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
  fullResultShort <- foreach (i=1:15, .combine = 'cbind', .packages=c("nortest", "foreach", "trend", "outliers",
                                                                      "EnvStats", "randtests", "ggpmisc")) %dopar% {
    currentIndex <- referenceIndexesShort[i]
    print(currentIndex)
    # Prepare the sliding windows (15 steps)
    partialResult <- foreach (j=1:15, .combine = 'cbind') %do% {
      initialIndex <- currentIndex+j-1-shortWindowLength+j-1
      finalIndex <- currentIndex+j-1
      sensorDataChunk <- sensorDataFull [initialIndex:finalIndex]
      resultSlidingWindow <- outliers_analysis(sensorDataChunk)
      resultVarSlidingWindow <- variation_analysis(sensorDataChunk)

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
      grubbsValEDT <- getElement(resultSlidingWindow, "grubbsEDT")
      snhtValEDT <- getElement(resultSlidingWindow, "snhtEDT")
      pettittValEDT <- getElement(resultSlidingWindow, "pettittEDT")
      lanzanteValEDT <- getElement(resultSlidingWindow, "lanzanteEDT")
      buishandRValEDT <- getElement(resultSlidingWindow, "buishandREDT")
      buishandUValEDT <- getElement(resultSlidingWindow, "buishandUEDT")
      resAngleSNHT <- getElement(resultSlidingWindow, "angleSNHT")
      resAnglePettitt <- getElement(resultSlidingWindow, "anglePettitt")
      resAngleLanzante <- getElement(resultSlidingWindow, "angleLanzante")
      resAngleBR <- getElement(resultSlidingWindow, "angleBR")
      resAngleBU <- getElement(resultSlidingWindow, "angleBU")
      resCV <- getElement(resultVarSlidingWindow, "cv")
      resRunsVal <- getElement(resultVarSlidingWindow, "runs")
      resNumRuns <- getElement(resultVarSlidingWindow, "numRuns")
      resLjung <- getElement(resultVarSlidingWindow, "ljungBox")
      resIQR <- getElement(resultVarSlidingWindow, "IQR")

      # Check whether this sample contains error
      hasOriginalFailure <- 0
      if (any(1<=failureData[initialIndex:finalIndex])){
        hasOriginalFailure <- 1
      }

      # Leave the results in a list, in memory, for combination with the other loops results
      myFullReturnList <- list("initialIndex" = initialIndex, "finalIndex" = finalIndex,
                               "shapiro" = shapiroVal, "ad" = adVal, "normality" = normalityAgreed,
                               "trend" = dataTrend, "outliers" = detectedOutliers, "snht" = SNHTVal,
                               "grubbs" = grubbsVal, "pettitt" = pettittVal, "lanzante" = lanzanteVal,
                               "buishandU" = buishandUVal, "buishandRange" = buishandRangeVal,
                               "grubbsEDT" = grubbsValEDT, "snhtEDT" = snhtValEDT, "pettittEDT" = pettittValEDT,
                               "lanzanteEDT" = lanzanteValEDT, "buishandREDT" = buishandRValEDT,
                               "buishandUEDT" = buishandUValEDT, "angleSNHT" = resAngleSNHT,
                               "anglePettitt" = resAnglePettitt, "angleLanzante" = resAngleLanzante,
                               "angleBR" = resAngleBR, "angleBU" = resAngleBU, "originalFailure" = hasOriginalFailure,
                               "cv" = resCV, "runs" = resRunsVal, "numRuns" = resNumRuns, "ljungBox" = resLjung,
                               "IQR" = resIQR)
    }
    #partialResult

  }
})

t5 <- system.time({
stopCluster(myCluster)

# Save the results to a CSV file
dfSave2 <- data.frame(matrix(unlist(fullResultShort[,1:225]), nrow=225, byrow=TRUE),stringsAsFactors=FALSE)
colnames(dfSave2) <- c("initialIndex", "finalIndex", "shapiro", "ad", "normality", "trend", "outliers",
                      "snht", "grubbs", "pettitt", "lanzante", "buishandU", "buishandRange", "grubbsEDT",
                      "snhtEDT", "pettittEDT", "lanzanteEDT", "buishandREDT", "buishandUEDT", "angleSNHT",
                      "anglePettitt", "angleLanzante", "angleBR", "angleBU", "originalFailure", "cv", "runs",
                      "numRuns", "ljungBox", "IQR")
write.table(dfSave2, file="C:/PhD/integratedProcessShort.csv", append= T, sep=',', row.names = FALSE)
})

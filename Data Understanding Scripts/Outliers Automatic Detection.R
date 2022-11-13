########################################################################################
## Script for automating the process of identifying outliers using the angles of the
## local maximum and minimum in several statistics as reference.
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

library(trend)
library(randtests)
library(outliers)
library(nortest)
library(EnvStats)
library(ggpmisc)
library(MLmetrics)

# Function that calculates all the angles in local maximum/minimum points 
turning_points_analysis <- function (testData) {
  # First of all, if all values are equal, the tests fail -> Return angle 0
  if (length(unique(testData))==1){
    print("Unique Value!")
    myReturnList <- list ("numMax" = 0, "numMin" = 0, "maxPositions" = 0,
                          "minPositions" = 0, "angles" = list(0))
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
    plot(x,(m1*x+n1),col='red', type='l', ylim = plot_limit) 
    lines(x,(m2*x+n2),col='green')
    lines(testData, col='blue', type='p')
    
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

data_transformation <- function (model, dataResult, initialIndex, lastIndex) {
  # Create empty lists in case there are no angles
  angles_max <- NA
  angles_min <- NA
  angles_max_conv <- NA
  angles_min_conv <- NA 
  errorResult <- FALSE
  
  # Assign angles to each category depending on their existence or not, and transform them
  if(dataResult$numMax > 0) {
    angles_max <- dataResult$angles[1:dataResult$numMax]
    #angles_max_conv <- (-180 - as.numeric(unlist(angles_max))) * -1
  }
  
  if(dataResult$numMin > 0) {
    angles_min <- dataResult$angles[(1 + dataResult$numMax):length(dataResult$angles)]
    #angles_min_conv <- 180 - as.numeric(unlist(angles_min))
  }
  
  # Create result object
  dfSave <- data.frame(
    "initial_index" = initialIndex, 
    "last_index" = lastIndex, 
    "model" = model, 
    "numMax" = dataResult$numMax, 
    "numMin" = dataResult$numMin,
    "maxPositions" = toString(dataResult$maxPositions), 
    "minPositions" = toString(dataResult$minPositions), 
    "angles_max" = toString(angles_max), 
    "angles_min" = toString(angles_min),
    "is_error" = errorResult
  )
  
  return(dfSave)
}

# Fault Injection
fullSensorData <- read.csv('C:/PhD/FaultInjectionDatasets/sensorscope/interpolated/injected_bias/mote=\'15\'_sensortype=temperature_faulttype=bias.txt', sep = ",", header = TRUE)
fullSensorData <- read.csv('C:/PhD/FaultInjectionDatasets/sensorscope/interpolated/injected_drift/mote=\'15\'_sensortype=temperature_faulttype=drift.txt', sep = ",", header = TRUE)
fullSensorData <- read.csv('C:/PhD/FaultInjectionDatasets/sensorscope/interpolated/injected_malfunction/mote=\'15\'_sensortype=temperature_faulttype=malfunction.txt', sep = ",", header = TRUE)
fullSensorData <- read.csv('C:/PhD/FaultInjectionDatasets/sensorscope/interpolated/injected_random/mote=\'15\'_sensortype=temperature_faulttype=random.txt', sep = ",", header = TRUE)
sensorData <- fullSensorData[4]
failureData <- fullSensorData[,3]
plot(sensorData$temperature, col="chartreuse3")
plot(failureData, col="chartreuse3")

# Gyor Air Quality
fullSensorData <- read.csv('C:/PhD/HiDALGO_Bosch/1WeekAirQualityBosch_annotated_final.csv', sep = ";", header = TRUE, stringsAsFactors = FALSE)
fullSensorData <- fullSensorData[fullSensorData$deviceID==359072065634004,]
sensorData <- fullSensorData[4]
failureData <- fullSensorData[,11]
plot(sensorData$NO2_1_CORR, col="chartreuse3")

# Select 20 random chunks of data
windowSize <- 120
#numMetrics <- length(sensorData$temperature)
numMetrics <- length(sensorData$NO2_1_CORR)
myIndexList <- sample (1:(numMetrics-windowSize),20,replace=F)
myIndexList

# Start analysis
fullResultList <- rep(list(), 20)
failureOriginalList <- rep(0, 20)
snhtFailureList <- rep(0, 20)
pettittFailureList <- rep(0, 20)
buishandRFailureList <- rep(0, 20)
buishandUFailureList <- rep(0, 20)
lanzanteFailureList <- rep(0, 20)
failureResultList <- rep(0, 20)
for (i in 1:20) {
  # Select the sample and plot it
  initialIndex <- myIndexList[i]
  finalIndex <- initialIndex + windowSize
  lastMetrics <- sensorData [initialIndex:finalIndex,]
  plot(lastMetrics, col="chartreuse3")
  #hist(lastMetrics, breaks=6)
  
  # Check whether this sample contains error
  if (any(1<=failureData[initialIndex:finalIndex])){
    failureOriginalList[i] <- 1
  }
  
  # If the list of elements is a constant value, report bias error
  if (length(unique(lastMetrics))==1){
    print("Unique Value!")
    snhtFailureList[i] <- 1
    pettittFailureList[i] <- 1
    buishandRFailureList[i] <- 1
    buishandUFailureList[i] <- 1
    lanzanteFailureList[i] <- 1
    next
  }
  
  out = snh.test(lastMetrics, 3000)
  out
  plot(out)
  out2 = pettitt.test(lastMetrics)
  out2
  plot(out2)
  out3 = br.test(lastMetrics, m=3000)
  out3
  plot(out3)
  out4 = bu.test(lastMetrics, m=3000)
  out4
  plot(out4)
  out5 = lanzante.test(lastMetrics)
  out5
  plot(out5)
  
  # Analyse the outcomes from SNHT statistic, so we identify outliers
  snhtResult <- turning_points_analysis(out$data)
  if (any(45>=snhtResult$angles)){
    snhtFailureList[i] <- 1
  }
  dfSave <- data_transformation("SNHT", snhtResult, initialIndex, finalIndex)
  
  # Analyse the outcomes from Pettitt statistic, so we identify outliers
  pettittResult <- turning_points_analysis(out2$data)
  if (any(1.3>=pettittResult$angles)){
    pettittFailureList[i] <- 1
  }
  dfSave2 <- data_transformation("Pettitt", pettittResult, initialIndex, finalIndex)
  dfTotalSave <- rbind(dfSave, dfSave2)
  
  # Analyse the outcomes from Buishand Range statistic, so we identify outliers
  buishandrResult <- turning_points_analysis(out3$data)
  if (any(88>=buishandrResult$angles)){
    buishandRFailureList[i] <- 1
  }
  dfSave3 <- data_transformation("BuishandR", buishandrResult, initialIndex, finalIndex)
  dfTotalSave <- rbind(dfTotalSave, dfSave3)
  
  # Analyse the outcomes from Buishand U statistic, so we identify outliers
  buishandUResult <- turning_points_analysis(out4$data)
  if (any(88>=buishandUResult$angles)){
    buishandUFailureList[i] <- 1
  }
  dfSave4 <- data_transformation("BuishandU", buishandUResult, initialIndex, finalIndex)
  dfTotalSave <- rbind(dfTotalSave, dfSave4)
  
  # Analyse the outcomes from Lanzante statistic, so we identify outliers
  lanzanteResult <- turning_points_analysis(out5$data)
  if (any(1.2>=lanzanteResult$angles)){
    lanzanteFailureList[i] <- 1
  }
  dfSave5 <- data_transformation("Lanzante", lanzanteResult, initialIndex, finalIndex)
  dfTotalSave <- rbind(dfTotalSave, dfSave5)
  
  write.table(dfTotalSave, file="C:/PhD/AutOutliersDetection.csv", col.names = !file.exists("C:/PhD/AutOutliersDetection.csv"), 
              append= T, sep=',')
}

failureOriginalList
snhtFailureList
pettittFailureList
buishandRFailureList
buishandUFailureList
lanzanteFailureList

# Aggregate results (Pettitt-Lanzanta and Buishand R-U)
plFailureList <- as.numeric(pettittFailureList | lanzanteFailureList)
plFailureList
bruFailureList <- as.numeric(buishandRFailureList | buishandUFailureList)
bruFailureList

# Aggregate final results SNHT + P-L + BR-U
aggregatedFailures <- snhtFailureList + plFailureList + bruFailureList
aggregatedFailures
predictedFailures <- as.numeric(aggregatedFailures>=2)
predictedFailures

# Calculate validation metrics
#predictedFailures <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,1)
#failureOriginalList <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0)

ConfusionMatrix(y_pred=predictedFailures, y_true=failureOriginalList)
Accuracy(y_pred=predictedFailures, y_true=failureOriginalList)
Precision(y_pred=predictedFailures, y_true=failureOriginalList, positive = "1")
Recall(y_pred=predictedFailures, y_true=failureOriginalList, positive = "1")
FBeta_Score(y_pred=predictedFailures, y_true=failureOriginalList, positive = "1", beta=0.5)
AUC(y_pred=predictedFailures, y_true=failureOriginalList)
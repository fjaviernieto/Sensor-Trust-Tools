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
# For homogeneity tests
library(trend)
# For Grubbs test
library(outliers)
# For datasets transformations
library(bestNormalize)

exp_diff <- function(inputData){
  transform_result <- rep (0, (length(inputData)-1))
  for (i in 2:length(inputData)){
    input_diff <- inputData[i] - inputData[i-1]
    # transform_result[i] <- abs(input_diff) * 1.2^(abs(input_diff) - 15)
    transform_result[i] <- input_diff * 1.1^(abs(input_diff) - 15)
  }
    
  return(transform_result)
}


fullSensorData <- read.csv('C:/Users/A173797/OneDrive - Atos/Documents/Personal/Doctorado/Datos/FaultInjectionDatasets/intel/not-interpolated/injected_drift/mote=1_sensortype=temperature_faulttype=drift.txt', sep = ",", header = TRUE)
fullSensorData <- read.csv('C:/Users/A173797/OneDrive - Atos/Documents/Personal/Doctorado/Datos/FaultInjectionDatasets/intel/not-interpolated/injected_malfunction/mote=1_sensortype=temperature_faulttype=malfunction.txt', sep = ",", header = TRUE)
sensorData <- fullSensorData[,6]
failureData <- fullSensorData[,4]
plot(sensorData, col="green")
plot(failureData, col="red")

# Select 20 random chunks of data
numMetrics <- length(sensorData)
referenceIndexes <- sample (500:numMetrics,20,replace=F)
referenceIndexes

timeWindow <- 220
currentIndex <- referenceIndexes[2]
selectedMetrics <- sensorData [(currentIndex-timeWindow):currentIndex] 
selectedFailures <- failureData [(currentIndex-timeWindow):currentIndex]
hist(selectedMetrics, breaks=8)
plot(selectedMetrics, col="green")

# Transformations using polynomial regression (from 1 to 6 grade polynomials)
print("Normalizing the time series...")
x <- 1:(timeWindow+1)
predictionModel_1 <- lm(selectedMetrics~poly(x,1,raw=TRUE))
predictionModel_2 <- lm(selectedMetrics~poly(x,2,raw=TRUE))
predictionModel_3 <- lm(selectedMetrics~poly(x,3,raw=TRUE))
predictionModel_4 <- lm(selectedMetrics~poly(x,4,raw=TRUE))
predictionModel_5 <- lm(selectedMetrics~poly(x,5,raw=TRUE))
predictionModel_6 <- lm(selectedMetrics~poly(x,6,raw=TRUE))
prediction_1 <- predict(predictionModel_1,newdata=data.frame(x))
prediction_2 <- predict(predictionModel_2,newdata=data.frame(x))
prediction_3 <- predict(predictionModel_3,newdata=data.frame(x))
prediction_4 <- predict(predictionModel_4,newdata=data.frame(x))
prediction_5 <- predict(predictionModel_5,newdata=data.frame(x))
prediction_6 <- predict(predictionModel_6,newdata=data.frame(x))

summary(predictionModel_1)
summary(predictionModel_2)
summary(predictionModel_3)
summary(predictionModel_4)
summary(predictionModel_5)
summary(predictionModel_6)

# Plot the predictions to see how the transformation works
plot(selectedMetrics, col="green", main="Polynomial Regression", xlab="X - Index", ylab="Y - Measurements", lty=1)
curve(predict(predictionModel_1,newdata=data.frame(x)),add=T, col="blue", lty=2)
curve(predict(predictionModel_2,newdata=data.frame(x)),add=T, col="red", lty=3)
curve(predict(predictionModel_3,newdata=data.frame(x)),add=T, col="purple", lty=4)
curve(predict(predictionModel_4,newdata=data.frame(x)),add=T, col="brown", lty=5)
curve(predict(predictionModel_5,newdata=data.frame(x)),add=T, col="orange", lty=6)
curve(predict(predictionModel_6,newdata=data.frame(x)),add=T, col="black", lty=7)
legend(x = "topright", legend=c("Original", "Deg. 1", "Deg. 2", "Deg. 3", "Deg. 4", "Deg. 5", "Deg. 6"), 
       col=c("green", "blue", "red", "purple", "brown", "orange", "black"), lty=1:7)

# Transformation using one of the predictions
selectedNormalizedMetrics_1 <- selectedMetrics - prediction_1
selectedNormalizedMetrics_2 <- selectedMetrics - prediction_2
selectedNormalizedMetrics_3 <- selectedMetrics - prediction_3
selectedNormalizedMetrics_4 <- selectedMetrics - prediction_4
selectedNormalizedMetrics <- selectedMetrics - prediction_5
selectedNormalizedMetrics_6 <- selectedMetrics - prediction_6

plot(selectedNormalizedMetrics_1, col="green")
plot(selectedNormalizedMetrics_2, col="green")
plot(selectedNormalizedMetrics_3, col="green")
plot(selectedNormalizedMetrics_4, col="green")
plot(selectedNormalizedMetrics, col="green", main="Polynomial Regression Transformation", xlab="X - Index", ylab="Y - Transformed Measurements")
plot(selectedNormalizedMetrics_6, col="green")
hist(selectedNormalizedMetrics, breaks=8)

# Transformation diffs level 1 to 3 (linear)
sensorData_diff1 <- diff(selectedMetrics, differences = 1)
sensorData_diff2 <- diff(selectedMetrics, differences = 2)
sensorData_diff3 <- diff(selectedMetrics, differences = 3)

plot(selectedMetrics, col="green", main="Selected Measurements", xlab="X - Index", ylab="Y - Measurements")
plot(sensorData_diff1, col="blue", main="Difference 1 Transformation", xlab="X - Index", ylab="Y - Transformed Measurements")
plot(sensorData_diff2, col="red", main="Difference 2 Transformation", xlab="X - Index", ylab="Y - Transformed Measurements")
plot(sensorData_diff3, col="purple", main="Difference 3 Transformation", xlab="X - Index", ylab="Y - Transformed Measurements")
hist(sensorData_diff1, breaks=8)

# Transformation with exponential function
sensorData_exp <- exp(selectedMetrics)
plot(sensorData_exp, col="blue")

# Transformation diff with exponential correction function
sensorData_diffExp <- exp_diff(selectedMetrics)
plot(sensorData_diffExp, col="blue", main="Exponential Difference Transformation", xlab="X - Index", ylab="Y - Transformed Measurements")
hist(sensorData_diffExp, breaks=8)

# Transformation Yeo Johnson
sensorData_yeo <- yeojohnson(selectedMetrics)$x.t
# sensorData_yeo_predict <- predict(predict_yeo)
# sensorData_yeo <- predict(predict_yeo, newdata=selectedMetrics)
plot(sensorData_yeo, col="green")
hist(sensorData_yeo, breaks=8)

# Transformation log(x)
sensorData_log <- log_x(selectedMetrics)$x.t
# sensorData_log_predict <- predict(predict_log)
# sensorData_log <- predict(predict_log, newdata=sensorData_log_predict)
plot(sensorData_log, col="green")
hist(sensorData_log, breaks=8)

# Transformation sqrt(x)
sensorData_sqrt <- sqrt_x(selectedMetrics)$x.t
# sensorData_sqrt_predict <- predict(predict_sqrt)
# sensorData_sqrt <- predict(predict_sqrt, newdata=sensorData_sqrt_predict)
plot(sensorData_sqrt, col="green")
hist(sensorData_sqrt, breaks=8)

plot(selectedNormalizedMetrics, col="green", main="Normalized Temperature")

out = snh.test(selectedMetrics, 3000)
outn = snh.test(selectedNormalizedMetrics, 3000)
outn2 = snh.test(sensorData_diffExp, 3000)
snhtOriginal = out$data
snhtNormalized = outn$data
snhtNormalized2 = outn2$data
plot(snhtOriginal, col="green")
lines(snhtNormalized, col="blue")
lines(snhtNormalized2, col="red")
print(out)
print (outn)
print (outn2)

out3 = br.test(selectedMetrics, m=3000)
out3n = br.test(selectedNormalizedMetrics, m=3000)
out3n2 = br.test(sensorData_diffExp, m=3000)
brOriginal = out3$data
brNormalized = out3n$data
brNormalized2 = out3n2$data
plot (brOriginal, col="green")
lines (brNormalized, col="blue")
lines (brNormalized2, col="red")
print(out3)
print(out3n)
print(out3n2)

out2 = pettitt.test(selectedMetrics)
out2n = pettitt.test(selectedNormalizedMetrics)
out2n2 = pettitt.test(sensorData_diffExp)
pettittOriginal = out2$data
pettittNormalized = out2n$data
pettittNormalized2 = out2n2$data
plot(pettittOriginal, col="green")
lines(pettittNormalized, col="blue")
lines(pettittNormalized2, col="red")
print(out2)
print(out2n)
print(out2n2)

out8 = grubbs.test(selectedMetrics)
out8n = grubbs.test(selectedNormalizedMetrics)
out8n2 = grubbs.test(sensorData_diffExp)
print(out8)
print(out8n)
print(out8n2)


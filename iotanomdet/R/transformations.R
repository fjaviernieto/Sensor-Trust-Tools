#' @title Exponential difference transformation
#'
#' @description This function takes a time series as an input and it transforms the data in such a way it
#' highlights the extreme values of the time series. It is based on the diff transformation, applying an exponential
#' factor according to the formula: yi(x) = (xi-x(i-1)) * 1.1^(abs(xi-x(i-1)) - 15)
#'
#' @param inputData A list of numbers containing a time series
#'
#' @return List containing the input data transformed with the exponential difference transformation
#' @export
#'
#' @examples
#' myList <- sample (1:5,10,replace=TRUE)
#' exp_diff(myList)
exp_diff <- function(inputData){
  transform_result <- rep (0, (length(inputData)-1))
  for (i in 2:length(inputData)){
    input_diff <- inputData[i] - inputData[i-1]
    # transform_result[i] <- abs(input_diff) * 1.2^(abs(input_diff) - 15)
    transform_result[i] <- input_diff * 1.1^(abs(input_diff) - 15)
  }

  return(transform_result)
}

#' @title Polynomial regression transformation
#'
#' @description This function takes a time series as input and it transforms the data using polynomial regression.
#' First of all, it creates a prediction model with polynomial regression of grade 5 and, then, it generates a
#' prediction based on the model. Finally, it subtracts the prediction from the original time series.
#'
#' @param inputData A list of numbers containing a time series
#'
#' @return List containing the input data transformed with the polynomial regression transformation
#' @export
#'
#' @examples
#' myList <- sample (1:5,10,replace=TRUE)
#' poly_reg(myList)
#'
#' @importFrom stats lm predict
poly_reg <- function(inputData){
  x <- 1:(length(inputData))
  predictionModel <- stats::lm(inputData~poly(x,5,raw=TRUE))
  prediction <- stats::predict(predictionModel,newdata=data.frame(x))
  transform_result <- inputData - prediction
  return(transform_result)
}

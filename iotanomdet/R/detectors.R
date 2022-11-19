library(ggpmisc)

# Bring the find_peaks function from ggpmisc to avoid warnings when building the package
find_peaks<-utils::getFromNamespace("find_peaks", "ggpmisc")

#' Angles calculation in turning points
#'
#' @param testData A list of numbers containing a time series. Expected to be the result of a homogeneity test
#'
#' @return List of elements that contain information about the number of angles, their position and calculation
#' @export
#'
#' @examples
#' sensorData <- sample (1:10,30,replace=TRUE)
#' turning_points_analysis(sensorData)
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
  # ggplot(data = dataShow, aes(x=x, y=y)) + geom_line() + stat_peaks(span=5, strict=TRUE, col = "red") +
  #  stat_valleys(span=5, strict=TRUE, col = "blue")

  # Extract max and min from the data and their position in the time series
  # maximos <- ggpmisc:::find_peaks(dataShow$y, span=5)
  maximos <- find_peaks(dataShow$y, span=5)
  pos_max <- which(maximos==TRUE)
  # minimos <- ggpmisc:::find_peaks(-dataShow$y, span=5)
  minimos <- find_peaks(-dataShow$y, span=5)
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

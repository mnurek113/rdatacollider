#' Start Large R Data Collider data acquisition
#'
#' @param acquisitionThreshold Numeric value for threshold applied to every acquisition channel (or every n-th acquisition channel when n-element vector of threshold values applied)
#' @param numberOfRuns number of acquisition runs
#' @param numberOfActiveChannels number of active channels

#' @param dataAcquisitionObj object of class DataAcquisition
#' @return A DataAcquistion object.
#' @examples
#' LargeRDataCollider.startDataAcquisition(20, numberOfActiveChannels = 3)
#' LargeRDataCollider.startDataAcquisition(c(20,25))

LargeRDataCollider.startDataAcquisition <- function (acquisitionThreshold,
                                                     numberOfRuns = 10,
                                                     numberOfActiveChannels = 10,
                                                     dataAcquisitionObj = NULL) {

  stopifnot(is.numeric(numberOfRuns), is.numeric(numberOfActiveChannels))
  stopifnot(numberOfRuns > 0)
  stopifnot(numberOfActiveChannels <= 10)

  if(is.null(dataAcquisitionObj)) {

    LargeRDataCollider.numberOfDetectorChannels <- 10
    dataAcquisitionObj <- structure(rep(0,numberOfActiveChannels),
                                    numberOfChannels = LargeRDataCollider.numberOfDetectorChannels,
                                    runStarted = TRUE,
                                    countsPerRun = vector(),
                                    meanAmplitudeValues = vector(),
                                    stdAmplitudeValues = vector(),
                                    class = "DataAcquisition")
  }

  countValues = rep(0, numberOfRuns)
  stdAmplitudeValues = rep(0, numberOfRuns)
  meanAmplitudeValues = rep(0, numberOfRuns)
  for (n in 1 : numberOfRuns) {

    countsPerChannel <- dataAcquisitionObj

    amplitudePerChannel <- rnorm(numberOfActiveChannels, 20, 5)

    dataAcquisitionObj <- suppressWarnings(countsPerChannel + as.numeric(amplitudePerChannel[1:numberOfActiveChannels] > acquisitionThreshold))
    countValues[n] <- sum(dataAcquisitionObj, na.rm = TRUE)
    meanAmplitudeValues[n] <- mean(amplitudePerChannel)
    stdAmplitudeValues[n] <- sd(amplitudePerChannel)
  }

  attr(dataAcquisitionObj, "countsPerRun") <- append(attr(dataAcquisitionObj, "countsPerRun"), countValues)
  attr(dataAcquisitionObj, "meanAmplitudeValues") <- append(attr(dataAcquisitionObj, "meanAmplitudeValues"), meanAmplitudeValues)
  attr(dataAcquisitionObj, "stdAmplitudeValues") <- append(attr(dataAcquisitionObj, "stdAmplitudeValues"), stdAmplitudeValues)

  dataAcquisitionObj
}

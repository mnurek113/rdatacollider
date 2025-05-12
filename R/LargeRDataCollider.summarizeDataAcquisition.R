#' Summarize Large R Data Collider data acquisition
#'
#' @param dataAcquisitionObj object of class DataAcquisition
#' @param display A flag indicating wheter to print out the summary in the console or not
#' @param saveData A flag indicating wheter to save the amplitude summary data in the file or not
#' @return
#' @examples
#' LargeRDataCollider.summarizeDataAcquisition <- function(dataAcquisition)
#' LargeRDataCollider.summarizeDataAcquisition <- function(dataAcquisition, display = FALSE)

LargeRDataCollider.summarizeDataAcquisition <- function(dataAcquisitionObj,
                                                        display = TRUE,
                                                        saveData = TRUE) {
  stopifnot(!is.null(dataAcquisitionObj))

  meanAmplitudeValues <- attr(dataAcquisitionObj, "meanAmplitudeValues")
  stdAmplitudeValues <- attr(dataAcquisitionObj, "stdAmplitudeValues")

  if(display) {
    print(paste(c("Counts per channel are:", dataAcquisitionObj[1:10]), collapse = " "))
    print(paste("Mean amplitude value for detector is: ", as.character(mean(meanAmplitudeValues))))
    print(paste("Standard deviation of amplitude values for detector is: ", as.character(mean(stdAmplitudeValues))))
  }

  if(saveData) {
    amplitudeDataFrame <- data.frame(mean = meanAmplitudeValues, std = stdAmplitudeValues)
    write.table(amplitudeDataFrame, "amplitude_analysis.RData")
  }
}

#' Plot histograms of mean and standard deviation of the amplitude values calculated for every single acquisition run
#'
#' @param amplitudeDataFrame DataFrame which contains mean and standard deviation of the amplitude values calculated for every single acquisition run
#' @return A figure that contains histograms for mean and standard deviation values
#' @examples
#' LargeRDataCollider.plotHistograms(amplitudeDataFrame)


LargeRDataCollider.plotHistograms <- function(amplitudeDataFrame) {

  p1 <- ggplot2::ggplot(amplitudeDataFrame) + ggplot2::geom_histogram(ggplot2::aes(x = mean))
  p2 <- ggplot2::ggplot(amplitudeDataFrame) + ggplot2::geom_histogram(ggplot2::aes(x = std))
  patchwork::wrap_plots(p1,p2)

}

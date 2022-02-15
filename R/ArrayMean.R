#' Finds the mean of a data frame
#'
#' @param dataframe data frame
#'
#' @return Mean of the designated data frame
#' @examples
#' \dontrun{OutlierDetect(exampledata)}
#' @export
ArrayMean <- function(dataframe) {

  columnmeans=colMeans(dataframe)
  totalmean=mean(dataframe)
  dataframe

}


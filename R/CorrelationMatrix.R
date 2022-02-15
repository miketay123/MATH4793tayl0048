#' Finds the correlation matrix of a two variables
#' @param dataframe data frame
#'
#' @return Mean of the designated data frame
#' @examples
#' \dontrun
#' {
#' CorrelationMatrix(exampledata)
#' }
#' @export
CorrelationMatrix <- function(dataframe) {

  columnmeans=colMeans(dataframe)
  totalmean=mean(columnmeans)
  totalmean

}


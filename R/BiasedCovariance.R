#' Finds the covariance of two variables in a data frame
#' Must first separate columns into desired variable as shown below
#' @param dataframe data frame
#' @param var1 first variable
#' @param var2 second variable
#'
#' @return covariance of variables
#' @export
BiasedCovariance <- function(var1, var2) {

  x=var1
  y=var2
  mx=mean(x)
  sx=sd(x)
  my=mean(y)
  sy=sd(y)

  diffsumsqx=(sx^2)/length(x)
  diffsumsqy=(sy^2)/length(y)

  cov=(mean(x)*mean(y))/(sd(x)*sd(y))
  cov

}


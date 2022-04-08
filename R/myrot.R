#' Finds the mean of a data frame
#'
#' @param mat data matrix
#'
#' @return angle of rotation for independence, eigenvalues, eigenvectors, plot with ellipse and principle axis, orthogonality test
#' @export

myrot <- function(mat)
{

  x1=mat[,1]
  x2=mat[,2]
  covmat=cov(mat)
  eigen1x=orthov1[1][1]
  eigen1y=orthov1[2][1]
  eigen_all=eigen(covmat)
  values=eigen_all$values
  angle=atan((eigen1y)/(eigen1x))

  a=ellipse(covmat, centre=c(mean(x1),mean(x2)), npoints=10000, col="red")
  e1=a[,1]
  e2=a[,2]
  plot(x1, x2, xlim=c(1.1*min(e1), 1.1*max(e1)), ylim=c(1.1*min(e2),1.1*max(e2)))
  points(a, type="l")
  abline(v=0,h=0)


  angle

  values[1][1]

  values[2][1]

  returnline=c(angle, values[1][1], values[2][1], orthov1, orthov2)



  slopev1=(orthov1[2][1])/(orthov1[1][1])
  slopev2=(orthov2[2][1])/(orthov2[1][1])
  abline(a=0, b=slopev1)
  abline(a=0, b=slopev2)

  return(list(rot=angle, lambda1=values[1][1], lambda2=values[2][1], e1=orthov1, e2=orthov2, orthogonality_test=slopev1*slopev2))
}

#' Displays a rotated ellipse that fits the data
#' @param cov Covariance Matrix
#' @param n number of values in generated data set
#' @param mu center of generated data set
#' @param theta rotation in radians
#'
#' @return Generated S, S tilde, and Theta
#' @export
#'
#'
mysik <- function(mu, cov, n, theta) {

  library(mvtnorm)
  library(ggplot2)

  mat=rmvnorm(n,mean=mu)
  sn=cov(mat)
  x1=mat[,1]
  x2=mat[,2]
  df = data.frame(x1,x2)

  s11tilde=sn[1,1]*(cos(theta))^2+sn[2][1]*sin(2*theta)+sn[2,2]*(sin(theta))^2
  s12tilde=sn[1,1]*cos(theta)*sin(theta)-sn[1,2]*cos(theta)^2+sn[1,2]*sin(theta)^2-sn[2,2]*cos(theta)*sin(theta)
  s22tilde=sn[1,1]*(sin(theta))^2-sn[2][1]*sin(2*theta)+sn[2,2]*(cos(theta))^2

  x1t=x1*cos(theta)+x2*sin(theta)
  x2t=-x1*sin(theta)+x2*cos(theta)

  stilde=matrix(c(s11tilde,s12tilde,s12tilde,s22tilde),nr=2,nc=2,byrow=TRUE)
  theta=0.5*atan((2*sn[2,1])/(sn[1,1]-sn[2,2]))

sn
stilde
  x=plot(x1, x2)
  z=ellipse(sn)
  a=plot(z)
  z1=ellipse(stilde)
  b=plot(z1)

  df = data.frame(x1,x2)
  library(ggplot2)
  g = ggplot(df, aes(x=x1,y=x2, col = I("Red")))+
    geom_point() +
    stat_ellipse()
  g
}


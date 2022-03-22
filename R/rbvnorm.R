#' GIBBS BIVARIATE SIMULATION
#' @param x2seed seed
#' @param iter iterations
#' @param mu mu for x1 and x2
#' @param sigma variance/covariance matrix
#'
#' @return matrix with iter rows, x1, x2
#' @export
#'

rbvnorm=function(x2seed=1, iter=10, mu, sigma)
{
  s12=sigma[1,2]
  s21=s12
  s11=sigma[1,1]
  s22=sigma[2,2]

  mean1=mu[1]
  mean2=mu[2]

  x2=x2seed
  count=0
  row=1
  datamat=matrix(nrow=iter, ncol=3)
  while (count<iter)
  {
    x1mean=mean1+s12*(1/s22)*(x2-mean2)
    x1sd=s11-(s12*(1/s22)*s12)
    x1=rnorm(1, mean=x1mean, sd=x1sd)

    x2mean=mean2+s12*(1/s22)*(x1-mean1)
    x2sd=s11-(s12*(1/s22)*s12)
    x2=rnorm(1, mean=x2mean, sd=x2sd)
    count=count+1
    entry=c(count, x1, x2)
    datamat[row,]=entry
    row=row+1
    #return(c(x1, x2, count))
  }
  invisible(list(gibbs = df , iter = iter, mu = mu, sigma = sigma))
  #return(list(x1final= x1, x2final=x2, total_iter=count))
  return(datamat)
}

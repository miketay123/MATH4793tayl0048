#' Finds whether or not a data frame is consistent with multivariate normality
#'
#' @param x data
#' @param sigma number of standard deviations
#'
#' @return Barplot or result 4.29, for given number of sd
#' @export



normal_mvn=function(x, sigma)
{
  library(ggplot2)
  obs_count=nrow(x)
  columns=ncol(x)
  datamat=matrix(nrow=1, ncol=columns)
  column_count=0
  resultmat=matrix(nrow=1, ncol=columns)

  empirical_rule_generator=function(sd_count, iter=20) {

    sigma=sd_count
    count=0
    data=matrix(nrow=iter, ncol=1)

    while(count <= iter)
    {
      x=rnorm(1000000)
      low=-sigma*sd(x)
      high=sigma*sd(x)
      in_interval=length(which(x > low & x < high))
      ratio=in_interval/1000000
      data[count,]=ratio
      count=count+1
    }
    mean(data)
  }
  p=empirical_rule_generator(sd_count=sigma, iter=30)
  q=1-p

  while(column_count<=columns)
  {
    dat=x[,column_count]
    dat=as.numeric(dat)
    mean_adjusted=dat-mean(dat)
    sdev=sd(mean_adjusted)

    low=-sigma*sd(mean_adjusted)
    high=sigma*sd(mean_adjusted)
    interval=c(low,high)
    in_interval=length(which(mean_adjusted > low & mean_adjusted < high))

    ratio=(in_interval)/(obs_count)-p
    norm_ind=3*sqrt((p*q)/obs_count)
    indicator=ratio-norm_ind
    resultmat[1,column_count]=ratio
    column_count=column_count+1
  }

  trigger=any(resultmat < 0)

  if(trigger == "TRUE")
  {
    barplot(resultmat, main="The data is not consistent with multivariate normality")
  }

  if(trigger == "FALSE")
  {
    barplot(resultmat, main="The data is consistent with multivariate normality")
  }

}

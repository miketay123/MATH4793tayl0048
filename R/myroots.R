#' Finds the correlation matrix of a two variables
#' @param mat data matrix
#'
#' @return roots
#' @export
#'


myroots=function(mat)
{
  n = dim(mat)[1]
  Sn=cov(mat)*(n-1)/n
  s11=Sn[1,1]
  s22=Sn[2,2]
  s12=Sn[1,2]
  myfun=function(x) (s22-s11)*sin(x)*cos(x) +s12*(cos(x)^2-sin(x)^2)
  myfun2=function(x) (s22-s11)*1/2*sin(2*x) + s12*(cos(2*x))

my.newt=function(x0,f=myfun,delta=1e-12,epsilon=1e-12,th1=c(0,2*pi),th2=c(0,pi/2),parameter=expression(theta)){

  fdash=function(x) (f(x+delta)-f(x))/delta

  d=1000
  i=0
  x=c()
  y=c()
  x[1]=x0
  y[1]=f(x[1])
  while(d > epsilon & i<100){
    i=i+1
    x[i+1]=x[i]-f(x[i])/fdash(x[i])
    y[i+1]=f(x[i+1])
    d=abs(y[i+1])
  }

  layout(matrix(1:2,nr=2,nc=1,byrow=TRUE),heights=c(3,4))
  curve(f(x), xlim=th1,xlab=parameter,ylab="f",main="myfun")
  abline(h=0,col="Red",lwd=2)

  curve(f(x),xlim=th2,xaxt="n", xlab=parameter,ylab="f",main=  "Newton-Raphson Algorithm")
  points(x,y,col="Red",pch=19,cex=0.5)

  axis(1,x,round(x,2),las=2)
  abline(h=0,col="Red")

  segments(x[1:(i-1)],y[1:(i-1)],x[2:i],rep(0,i-1),col="Blue",lwd=0.5)
  segments(x[2:i],rep(0,i-1),x[2:i],y[2:i],lwd=0.5,col="Green")
  arrows(x0=x[i+1],y0=y[1],x1=x[i+1],y1=y[i+1])
  text(x[i+1],y[1],x[i+1])
  nn=length(x)
  list(x=x,y=y,d=d, root=x[nn])

}
my.newt(1.1,f = myfun, th2=c(0,6))
}

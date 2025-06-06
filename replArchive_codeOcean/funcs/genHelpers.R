char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }
substrRight = function(x, n){ substr(x, nchar(x)-n+1, nchar(x)) }
# https://cran.r-project.org/doc/contrib/Lemon-kickstart/rescale.R
rescale<-function(x,newrange) {
 if(nargs() > 1 && is.numeric(x) && is.numeric(newrange)) {
  # if newrange has max first, reverse it
  if(newrange[1] > newrange[2]) {
   newmin<-newrange[2]
   newrange[2]<-newrange[1]
   newrange[1]<-newmin
  }
  xrange<-range(x)
  if(xrange[1] == xrange[2]) stop("can't rescale a constant vector!")
  mfac<-(newrange[2]-newrange[1])/(xrange[2]-xrange[1])
  invisible(newrange[1]+(x-xrange[1])*mfac)
 }
 else {
  cat("Usage: rescale(x,newrange)\n")
  cat("\twhere x is a numeric object and newrange is the min and max of the new range\n")
 }
}
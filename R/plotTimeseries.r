#' Plot as time series
#'
#' Plots the output of a dynamic model as a time series.
#'
#' @param names Names of items (i.e. variables or process rates) to plot.
#' @param lty Line types, should be of the same length as \code{names}.
#' @param col Line colors, should be of the same length as \code{names}.
#' @param ofile Name of the outout file (with extension).
#' @inheritParams plotProfiles
#'
#' @return \code{NULL}
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#' times= 0:100
#' vars= c("a","b","c")
#' values= matrix(runif(length(times)*length(vars)), nrow=length(times))
#' dyn= cbind(times, values)
#' colnames(dyn)= c("time", vars)
#' plotTimeseries(c("a","c"), dyn, timeconv=function(x){x}, ofile=tempfile())

plotTimeseries= function(names, dyn, xlab="", ylab="", xlim=NULL, ylim=NULL, 
  pos="top", lty=1:length(names), col=rep("black", length(names)),
  timeconv=function(x){ISOdatetime(1970,1,1,0,0,0)+x},
  width=4, height=4, pointsize=10,
  ofile="out.svg", fmt="svg", replace=TRUE, ...)
{
  t= timeconv(dyn[,"time"])
  xrng= if (is.null(xlim)) range(t) else xlim
  yrng= if (is.null(ylim)) range(dyn[,names]) else ylim
  if ((!replace) && file.exists(ofile))
    stop("file '",ofile,"' already exists")
  if (fmt=="svg")
    grDevices::svg(ofile, width=width, height=height, pointsize=pointsize)
  else if (fmt=="pdf")
    grDevices::pdf(ofile, width=width, height=height, pointsize=pointsize)
  else
    stop("graphics format '",fmt,"' not supported")
  graphics::plot(x=0, y=0, xlim=xrng, ylim=yrng, type="n", bty="n", xlab=xlab, ylab=ylab)
  for (i in 1:length(names)) {
    graphics::lines(t, dyn[,names[i]], lty=lty[i], col=col[i])
  }
  if (length(names) > 1)
    graphics::legend(pos, bty="n", col=col, lty=lty, legend=names, ...)
  grDevices::graphics.off()
  return(invisible(NULL))
}


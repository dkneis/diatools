#' Color Map of Dynamic Simulation Output
#'
#' Creates a color map from an object of class \code{sim1D}.
#'
#' @inheritParams sim1D.query
#'
#' @param xlab Label for the time axis (x).
#' @param ylab Label for the depth axis (y).
#' @param timeconv A function to convert numeric times to class \code{POSIXct}.
#' @param ... Further arguments to be passed to \code{fields::image.plot}. It is
#'   often useful to pass \code{legend.mar} at least.
#'
#' @return \code{NULL}
#'
#' @note The function calls \code{fields::image.plot} which is possibly
#'   incompatible with the \code{layout} to arrange multiple plots.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export

plot3d= function(obj, item, xlab="time", ylab="depth",
  rangeT=c(NA,NA), rangeX=c(NA,NA), partly=TRUE,
  timeconv=function(x){ISOdatetime(1970,1,1,0,0,0)+x}, ...)
{
  if (class(obj) != "sim1D")
    stop("'obj' is not an object of class 'sim1D'")   
  m= sim1D.query(obj, item, rangeT, rangeX, partly, attrib=TRUE)
  m= m[,ncol(m):1]
  t= timeconv(attr(obj, which="times", exact=TRUE))
  x= attr(obj, which="coordinates", exact=TRUE)
  d= -1*c(x[nrow(x):1,"max"], x[1,"min"])
  if (min(m) != max(m)) {
    fields::image.plot(x=t, y=d, z=m, xlab=xlab, ylab=ylab, ...)
  } else {
    graphics::plot(x=range(t), y=range(d), type="n", bty="n", xlab=xlab, ylab=ylab)
    graphics::legend("center", bty="n", legend=paste0("const at ",min(m)))
  }
  return(invisible(NULL))
}


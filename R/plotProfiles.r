#' Plot dynamic simulation output as vertical profiles
#'
#' Plots the output of a dynamic 1D diagenetic model as vertical profiles.
#'
#' @inheritParams plot3d
#' @param obs Data frame of observations. See notes below.
#' @param combine Combine data for all times in a single plot?
#' @param nc Maximum number of columns in layout. Only used if \code{combine} is
#'   \code{FALSE}. The default of \code{NULL} puts all plots in a single row.
#' @param pos Keyword specifying the position of the legend.
#' @param xlim Limits for the value axis (x). Computed if \code{NULL}.
#' @param ylim Limits for the depth axis (y). Computed if \code{NULL}.
#' @param ... Additional arguments to be passed to \code{legend}.
#'
#' @return \code{NULL}
#'
#' @note The data frame of observations passed as \code{obs} needs to
#'   have 3 columns named 'time', 'depth', and 'value'. If the times of
#'   observation do not match exactly with the model output, data for the
#'   nearest time points available in \code{dyn} will be plotted. Data in the
#'   'time' column must be of the same type as the return value of \code{timeconv}.
#'   It is OK if the 'depth' and 'value' columns contain \code{NA} only and in
#'   that case, just the simulated data are plotted.
#'   The \code{layout} mechanism is used to arrange figures if \code{combine} is
#'   \code{FALSE}, thus, nesting within other multi-figure setups won't work.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#' gr= makeGrid(dz0=0.01, dzMax=0.02, zMax=0.2, beta=1)
#' times= 0:2
#' values= matrix(runif(length(times)*nrow(gr)), nrow=length(times))
#' dyn= cbind(times, values)
#' colnames(dyn)= c("time", paste("X",1:nrow(gr),sep="."))
#' obs= data.frame(time=c(1,1,2,2,2), depth=c(0.03, 0.04, 0.03, 0.04, 0.05))
#' obs$value= runif(nrow(obs))
#' plotProfiles("X", dyn, gr, obs, timeconv=function(x){x})

plotProfiles= function(name, dyn, gr, obs, xlab="mmol/L", ylab="depth",
  xlim=NULL, ylim=NULL, combine=TRUE, nc=NULL,
  pos="top",
  timeconv=function(x){ISOdatetime(1970,1,1,0,0,0)+x}, ...)
{
  obsNames= c("time","depth","value")
  if (!all(obsNames %in% names(obs)))
    stop("observation table must have columns '",paste(obsNames, collapse="','"),"'")
  times= sort(unique(obs$time))
  t= timeconv(dyn[,"time"])
  it= round(approx(x=t, y=1:length(t), xout=times, rule=1)$y)
  if (any(is.na(it)))
    stop("no data for requested time(s): ",paste(times[is.na(it)],collapse=", "))
  m= dyn[it,paste(name,1:nrow(gr),sep=".")]
  d= -0.5 * (gr$zLw + gr$zUp)
  if (is.null(xlim))
    xrng= range(c(m, obs$value), na.rm=TRUE)
  else
    xrng= xlim
  if (is.null(ylim))
    yrng= range(c(d, -obs$depth))
  else
    yrng= ylim
  if (combine) {
    plot(x=0, y=0, xlim=xrng, ylim=yrng, type="n", bty="n", xlab=xlab, ylab=ylab)
    for (i in 1:length(it)) {
      lines(m[i,], d, lty=1, col=i)
      k= which(obs$time == times[i])
      points(obs$value[k], -obs$depth[k], col=i)
    }
    legend(pos, bty="n", lty=1, col=1:length(it), legend=times, ...)
  } else {
    if (is.null(nc))
      layout(matrix(1:length(it), nrow=1))
    else
      nr= ceiling(length(it)/nc)
      layout(matrix(1:(nr*nc), ncol=nc, byrow=TRUE))
    for (i in 1:length(it)) {
      label= (i == (nr-1)*nc+1) # axis labels only in lower left corner
      plot(x=0, y=0, xlim=xrng, ylim=yrng, type="n", bty="n",
        xlab=ifelse(label,xlab,""), ylab=ifelse(label,ylab,""))
      lines(m[i,], d)
      k= which(obs$time == times[i])
      points(obs$value[k], -obs$depth[k])
      legend(pos, bty="n", legend=times[i], ...)
    }
    layout(1)
  }
  return(invisible(NULL))
}


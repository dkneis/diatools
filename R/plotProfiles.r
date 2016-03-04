#' Plot dynamic simulation output as vertical profiles
#'
#' Plots the output of a dynamic 1D diagenetic model as vertical profiles.
#'
#' @inheritParams plot3d
#' @param obs Data frame of observations. See notes below.
#' @param pos Keyword specifying the position of the legend.
#' @param lty Line types, 1 value or a vector as long as \code{times}.
#' @param col Line colors, 1 value or a vector as long as \code{times}.
#' @param ... Additional arguments to be passed to \code{legend}.
#'
#' @return \code{NULL}
#'
#' @note The data frame of observations passed as \code{obs} needs to
#'   have 3 columns named 'time', 'depth', and 'value'. If the times of
#'   observation do not match exactly with the model output, data for the
#'   nearest time points available in \code{dyn} will be plotted.
#'   It is OK if the 'depth' and 'value' columns contain \code{NA} only and in
#'   that case, just the simulated data are plotted.
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
#' plotProfiles("X", dyn, gr, obs)

plotProfiles= function(name, dyn, gr, obs, xlab="mmol/L", ylab="depth",
  pos="center", lty=rep(1, length(times)), col=1:length(times), ...)
{
  obsNames= c("time","depth","value")
  if (!all(obsNames %in% names(obs)))
    stop("observation table must have columns '",paste(obsNames, collapse="','"),"'")
  times= sort(unique(obs$time))
  t= dyn[,"time"]
  it= round(approx(x=t, y=1:length(t), xout=times, rule=1)$y)
  if (any(is.na(it)))
    stop("no data for requested time(s): ",paste(times[is.na(it)],collapse=", "))
  m= dyn[it,paste(name,1:nrow(gr),sep=".")]
  d= -0.5 * (gr$zLw + gr$zUp)
  plot(x=range(c(m, obs$value), na.rm=TRUE), y=range(d),
    type="n", bty="n", xlab=xlab, ylab=ylab)
  for (i in 1:length(it)) {
    lines(m[i,], d, col=col[i])
    k= which(obs$time == times[i])
    points(obs$value[k], -obs$depth[k], col=col[i])
  }
  legend(pos, bty="n", lty=lty, col=col, legend=times, ...)
  return(invisible(NULL))
}


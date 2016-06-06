#' Plot dynamic simulation output as vertical profiles
#'
#' Plots the output of a dynamic 1D diagenetic model as vertical profiles.
#'
#' @inheritParams plot3d
#' @param obs Data frame of observations. See notes below.
#' @param pos Keyword specifying the position of the legend.
#' @param xlim Limits for the value axis (x). Computed if \code{NULL}.
#' @param ylim Limits for the depth axis (y). Computed if \code{NULL}.
#' @param width Plot width in inch.
#' @param height Plot height in inch.
#' @param pointsize Font size.
#' @param dir Output directory for generated graphics.
#' @param fmt Graphics format, either 'svg' or 'pdf'.
#' @param prefix Prefix put before the value of \code{name} when constructing
#'   file names.
#' @param replace If \code{FALSE} existing output files result in an error.
#' @param ... Additional arguments to be passed to \code{legend}.
#'
#' @return A vector holding the names of the generated graphics files.
#'
#' @note The data frame of observations passed as \code{obs} needs to
#'   have 3 columns named 'time', 'depth', and 'value'. If the times of
#'   observation do not match exactly with the model output, data for the
#'   nearest time points available in \code{dyn} will be plotted. Data in the
#'   'time' column must be of the same type as the return value of \code{timeconv}.
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
#' plotProfiles("X", dyn, gr, obs, timeconv=function(x){x}, dir=tempdir())

plotProfiles= function(name, dyn, gr, obs, xlab="mmol/L", ylab="depth",
  xlim=NULL, ylim=NULL, 
  pos="top",
  timeconv=function(x){ISOdatetime(1970,1,1,0,0,0)+x},
  width=4, height=4, pointsize=10,
  dir=".", fmt="svg", prefix="profile", replace=TRUE, ...)
{
  obsNames= c("time","depth","value")
  if (!all(obsNames %in% names(obs)))
    stop("observation table must have columns '",paste(obsNames, collapse="','"),"'")
  times= sort(unique(obs$time))
  t= timeconv(dyn[,"time"])
  it= round(stats::approx(x=t, y=1:length(t), xout=times, rule=1)$y)
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
  out=c()
  for (i in 1:length(it)) {
    ofile= paste0(dir,"/",prefix,name,"_",i,".",fmt)
    out= c(out, ofile)
    if ((!replace) && file.exists(ofile))
      stop("file '",ofile,"' already exists")
    if (fmt=="svg")
      grDevices::svg(ofile, width=width, height=height, pointsize=pointsize)
    else if (fmt=="pdf")
      grDevices::pdf(ofile, width=width, height=height, pointsize=pointsize)
    else
      stop("graphics format '",fmt,"' not supported")
    graphics::plot(x=0, y=0, xlim=xrng, ylim=yrng, type="n", bty="n", xlab=xlab, ylab=ylab)
    graphics::lines(m[i,], d)
    k= which(obs$time == times[i])
    graphics::points(obs$value[k], -obs$depth[k])
    graphics::legend(pos, bty="n", legend=times[i], ...)
    grDevices::graphics.off()
  }
  return(out)
}


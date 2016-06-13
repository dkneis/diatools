#' Goodness-of-fit for a 1D dynamic, multi-component model
#'
#' Compares observation data with the model output.
#'
#' @inheritParams sim1D.query
#' @param obs Data frame of observations. See notes below.
#' @param f A function taking two numeric vectors with observed and simulated
#'   data as input. It should return a numeric vector (which can be of length 1).
#'
#' @return A list with as many elements as there are corresponding variables in
#'   \code{dyn} and \code{obs}. Each element contains the output of \code{f}.
#'   If, for a particular variable, no corresponding data are found (because
#'   of a missing overlap of spatial and/or time interval), the list element for
#'   that variable is set to \code{NULL}.
#'   If there aren't any corresponding variables (due to a lack of matching
#'   names), the return value is a list of length 0.
#'
#' @note The data frame of observations passed as \code{obs} needs to
#'   have 4 columns named 'time', 'coordinate', 'variable', and 'value'.
#'   Entries in column 'time' are coerced to numbers using \code{as.numeric}.
#'   The corresponding simulated values are picked from the matching spatial
#'   box/layer and the nearest time point available in the model output.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#' gr <- makeGrid(dz0=0.01, dzMax=0.02, zMax=0.1, beta=1)
#' times <- 0:2
#' vars <- c("A","B")
#' pros <- c("X","Y")
#' values <- matrix(1:(length(times)*nrow(gr)*(length(vars)+length(pros))),
#'   nrow=length(times))
#' dyn <- cbind(times, values)
#' colnames(dyn) <- c("time", paste(rep(vars, each=nrow(gr)), 1:nrow(gr),sep="."),
#'   paste(rep(pros, each=nrow(gr)), 1:nrow(gr),sep="."))
#' obj <- sim1D.create(dyn, namesVars=vars, namesPros=pros, xMin=gr$zUp, xMax=gr$zLw)
#' obs <- data.frame(time=c(1,1,2,2), coordinate=c(0.03, 0.04, 0.05, 0.04),
#'   variable=c("X","X","X","Y"), value=0)
#' fun <- function(sim,obs) {c(bias=mean(sim-obs), rmse=mean((sim-obs)^2))}
#' gof(obj, obs, f=fun)

gof= function(obj, obs, f=function(sim,obs) {mean((sim-obs)^2)})
{
  obsNames= c("time","coordinate","variable","value")
  if (!all(obsNames %in% names(obs)))
    stop("observation data frame must have columns '",paste(obsNames, collapse="','"),"'")
  err= list()
  bad= which(!(obs$variable %in% attr(obj, which="items", exact=TRUE)))
  if (length(bad) > 0)
    stop("'obj' does not contain data for observed variable(s) '",
      paste(obs$variable[bad], collapse="', '"),"'")
  for (v in sort(unique(obs$variable))) {
    # Filter for current variable
    sim= sim1D.query(obj, item=v, rangeT=c(NA,NA), rangeX=c(NA,NA), partly=TRUE, attrib=TRUE)
    simT= attr(sim, which="times", exact=TRUE)
    simXmin= attr(sim, which="coordinates", exact=TRUE)[,"min"]
    simXmax= attr(sim, which="coordinates", exact=TRUE)[,"max"]
    obsSub= subset(obs, obs$variable == v, select=c("time","coordinate","value"))
    # Find time and space bins in simulated data corresponding to observations
    obsSub$iT= round(stats::approx(x=simT, y=1:length(simT),
      xout=as.numeric(obsSub$time), method="linear", rule=1)$y)
    obsSub$iX= stats::approx(x=c(simXmin,simXmax[length(simXmax)]), y=1:(length(simXmin)+1),
      xout=obsSub$coordinate, method="constant", rule=1, f=0)$y
    obsSub$iX[which(obsSub$iX > length(simXmin))]= length(simXmin) # match of rightmost boundary
    obsSub= subset(obsSub, (!is.na(obsSub$iT)) & (!is.na(obsSub$iX)))
    if (nrow(obsSub) > 0) {
      pos= (obsSub$iX-1)*nrow(sim)+obsSub$iT
      err[[v]]= f(sim=as.vector(sim)[pos], obs=obsSub$value)
      # Begin: Code for visual check of data matching
      #plot(range(simT), range(c(simXmin, simXmax)), type="n")
      #points(obsSub$time, obsSub$coordinate, col=1:nrow(obsSub), pch=1)
      #t= matrix(rep(simT, length(simXmin)), ncol=length(simXmin))
      #d= matrix(rep((simXmin+simXmax)/2, length(simT)), ncol=length(simXmin), byrow=TRUE)
      #points(as.vector(t)[pos], as.vector(d)[pos], col=1:length(pos), pch=4)
      # End: Code for visual check
    } else {
      err[[v]]= NULL
    }
  }
  return(err)
}


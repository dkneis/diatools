#' Goodness-of-fit for a 1D dynamic, multi-component model
#'
#' Compares observation data with the model output.
#'
#' @inheritParams plot3d
#' @param obs Data frame of observations. See notes below.
#' @param f A function taking two numeric vectors with observed and simulated
#'   data as input. It should return a numeric vector (which can be of length 1).
#'
#' @return A list with as many elements as there are corresponding variables in
#'   \code{dyn} and \code{obs}. Each element contains the output of \code{f}.
#'   If, for a particular variable, no corresponding data are found (because
#'   of a missing overlap of depth and/or time interval), the list element for
#'   that variable is set to \code{NULL}.
#'   If there aren't any corresponding variables (due to a lack of matching
#'   names), the return value is a list of length 0.
#'
#' @note The data frame of observations passed as \code{obs} needs to
#'   have 4 columns named 'time', 'depth', 'variable', and 'value'.
#'   Simulated values are assigned to the observation data based on a 
#'   nearest-neighbor principle, i.e. data are picked from the nearest bin with
#'   respect to both time and depth.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#' gr= makeGrid(dz0=0.01, dzMax=0.02, zMax=0.1, beta=1)
#' times= 0:2
#' vars= c("X","Y")
#' values= matrix(1:(length(times)*nrow(gr)*length(vars)), nrow=length(times))
#' dyn= cbind(times, values)
#' colnames(dyn)= c("time", paste("X",1:nrow(gr),sep="."),
#'   paste("Y",1:nrow(gr),sep="."))
#' obs= data.frame(time=c(1,1,2,2), depth=c(0.03, 0.04, 0.05, 0.04),
#'   variable=c("X","X","X","Y"), value=0)
#' fun= function(sim,obs) {c(bias=mean(sim-obs), rmse=mean((sim-obs)^2))}
#' gof(dyn, gr, obs, f=fun)

gof= function(dyn, gr, obs, f=function(sim,obs) {mean((sim-obs)^2)})
{
  obsNames= c("time","depth","variable","value")
  if (!all(obsNames %in% names(obs)))
    stop("observation table must have columns '",paste(obsNames, collapse="','"),"'")
  err= list()
  for (v in sort(unique(obs$variable))) {
    cols= which(grepl(pattern=paste0("^",v,"[.][0123456789]+$"), x=colnames(dyn)))
    if (length(cols) > 0) {
      if (length(cols) != nrow(gr))
        stop("number of selected columns does not match with grid dimension")
      # Filter for current variable
      dynSub= dyn[,cols]
      obsSub= subset(obs, obs$variable == v, select=c("time","depth","value"))
      # Assign model output to observations (nearest time and depth bin)
      obsSub$irow= round(approx(x=dyn[,"time"], y=1:nrow(dynSub), xout=obsSub$time, rule=1)$y)
      obsSub$icol= round(approx(x=(gr$zUp+gr$zLw)/2, y=1:ncol(dynSub), xout=obsSub$depth, rule=1)$y)
      obsSub= subset(obsSub, (!is.na(obsSub$irow)) & (!is.na(obsSub$icol)))
      if (nrow(obsSub) > 0) {
        pos= (obsSub$icol-1)*nrow(dynSub)+obsSub$irow
        err[[v]]= f(sim=as.vector(dynSub)[pos], obs=obsSub$value)
        # Begin: Code for visual check of data matching
        #plot(range(dyn[,"time"]), range((gr$zUp+gr$zLw)/2), type="n")
        #points(obsSub$time, obsSub$depth, col=1:nrow(obsSub), pch=1)
        #t= matrix(rep(dyn[,"time"], length(cols)), ncol=length(cols))
        #d= matrix(rep((gr$zUp+gr$zLw)/2, nrow(dynSub)), ncol=length(cols), byrow=TRUE)
        #points(as.vector(t)[pos], as.vector(d)[pos], col=1:length(pos), pch=4)
        # End: Code for visual check
      } else {
        err[[v]]= NULL
      }
    }
  }
  return(err)
}



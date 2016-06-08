
#' Create an Object of Class 'sim1D'
#'
#' Create an object of class 'sim1D' from the output of a dynamic, 1D,
#' rodeo-based model.
#'
#' @param dyn Numeric matrix output by the ODE solver
#'   (e.g. \code{\link[deSolve]{ode}}).
#' @param model An object of the \code{\link[rodeo]{rodeo-class}} representing
#'   the model used to create \code{dyn}.
#' @param xMin Minimum coordinates of the spatial boxes, e.g. top layer
#'   boundaries in a vertical 1D model.
#' @param xMax Maximum coordinates of the spatial boxes, e.g. bottom layer
#'   boundaries in a vertical 1D model.
#' @param keepItems Names of the items to be kept. Must be names of state
#'   variables or processes existing in \code{model}.
#' @param sep The character used in the column names of \code{dyn} to separate
#'   the variable/process name from the box/layer index.
#' @param rangeT Time range. Data outside this range are dropped.
#' @param rangeX Range of spatial coordinates (e.g. depths). Data outside this
#'   range are dropped.
#' @param nsig Number of significant digits to retain (integer). 
#'
#' @return An object of class 'sim1D'. Use \code{sim1D.query} to extract data
#'   from such objects.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export

sim1D.create <- function(dyn, model, xMin, xMax,
  keepItems=c(model$namesVars(),model$namesPros()), sep=".",
  rangeT=c(NA,NA), rangeX=c(NA,NA), nsig=2
) {
  if (class(model) != "rodeo")
    stop("'model' should be an object of class 'rodeo'")
  if (length(xMin) != length(xMax))
    stop("'xMin' and 'xMax' differ in length")
  if (any(xMin > xMax))
    stop("detected case(s) where 'xMin' > 'xMax'")
  if (!all(is.matrix(dyn), is.numeric(dyn)))
    stop("'dyn' should be a numeric matrix")
  if (colnames(dyn)[1] != "time")
    stop("expecting 'time' as first column of 'dyn'")
  if (ncol(dyn) != (1 + model$lenVars()*length(xMin) + model$lenPros()*length(xMin)))
    stop("unexpected number of columns in 'dyn'")
  if (!all(length(rangeT) == 2, length(rangeX) == 2))
    stop("'rangeT' and 'rangeX' should be vectors of length 2")
  # Time filter
  times <- dyn[,1]
  rangeT[1] <- if (is.na(rangeT[1])) -Inf else min(as.numeric(rangeT))
  rangeT[2] <- if (is.na(rangeT[2])) Inf else max(as.numeric(rangeT))
  keep <- (times >= rangeT[1]) & (times <= rangeT[2])
  dyn <- dyn[keep,]
  times <- times[keep]
  # Filter for variables and depth (and remove time column)
  rangeX[1] <- if (is.na(rangeX[1])) -Inf else min(as.numeric(rangeX))
  rangeX[2] <- if (is.na(rangeX[2])) Inf else max(as.numeric(rangeX))
  whichX <- which((xMax >= rangeX[1]) & (xMin <= rangeX[2]))
  keep <- c(paste(rep(keepItems, each=length(whichX)), whichX, sep=sep))
  if (!all(keep %in% colnames(dyn)))
    stop("requested 'keepItems' for selected ranges not available in 'dyn'")
  dyn <- dyn[,keep]
  xMin <- xMin[whichX]
  xMax <- xMax[whichX]
  # Round
  dyn <- signif(dyn, nsig)
  # Transform into array
  res <- list(
    .times= times,
    .x= cbind(min=xMin, max=xMax),
    .items= keepItems,
    .data= array(data=dyn, dim=c(length(times), length(whichX), length(keepItems)))
  )
  class(res) <- "sim1D"
  return(res)
}

#' Query an Object of Class 'sim1D'
#'
#' Query an object of class 'sim1D' holding the output of a dynamic, 1D,
#' rodeo-based model.
#'
#' @param obj Object created with \code{sim1D.create}.
#' @param item Name of a state variable or process (character string).
#' @param rangeT Time range. Data outside this range are dropped.
#' @param rangeX Range of spatial coordinates (e.g. depths). Data outside this
#'   range are dropped.
#' @param partly Logical. If \code{TRUE}, the result contains data for those
#'   boxes/layers that are partly within the range \code{rangeX}. If
#'   \code{FALSE}, only data for boxes/layers being entirely within
#'   \code{rangeX} are returned.
#'
#' @return A numeric matrix where rows represent time and columns represent
#'   boxes/layers. Row names are constructed from the time values. Column names
#'   are created by concatenating the boxes'/layers' minimum and maximum 
#'   coordinate, separated by comma.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export

sim1D.query <- function(obj, item, rangeT=c(NA,NA), rangeX=c(NA,NA), partly=FALSE) {
  if (class(obj) != "sim1D")
    stop("'obj' is not an object of class 'sim1D'")
  if (!all(length(item) == 1, is.character(item)))
    stop("'item' should be a scalar character string")
  if (!all(length(rangeT) == 2, length(rangeX) == 2))
    stop("'rangeT' and 'rangeX' should be vectors of length 2")
  # Find item index
  whichI <- which(obj$.items == item)
  if (length(whichI) != 1)
    stop("'obj' has no data for item '",item,"'")
  # Find time indices
  rangeT[1] <- if (is.na(rangeT[1])) -Inf else min(as.numeric(rangeT))
  rangeT[2] <- if (is.na(rangeT[2])) Inf else max(as.numeric(rangeT))
  whichT <- which((obj$.times >= rangeT[1]) & (obj$.times <= rangeT[2]))
  if (length(whichT) < 1)
    stop("'obj' has no data for time range ",rangeT[1]," - ",rangeT[2])
  # Find spatial indices
  rangeX[1] <- if (is.na(rangeX[1])) -Inf else min(as.numeric(rangeX))
  rangeX[2] <- if (is.na(rangeX[2])) Inf else max(as.numeric(rangeX))
  if (partly)
    whichX <- which((obj$.x[,"max"] >= rangeX[1]) & (obj$.x[,"min"] <= rangeX[2]))
  else
    whichX <- which((obj$.x[,"min"] >= rangeX[1]) & (obj$.x[,"max"] <= rangeX[2]))
  if (length(whichX) < 1)
    stop("'obj' has no data for x range ",rangeX[1]," - ",rangeX[2])
  # Return data matrix
  res <- obj$.data[whichT,whichX,whichI]
  dimnames(res) <- list(obj$.times[whichT],
    paste(obj$.x[whichX,"min"], obj$.x[whichX,"max"], sep=","))
  return(res)
}


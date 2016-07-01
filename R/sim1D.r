
#' Create an Object of Class 'sim1D'
#'
#' Create an object of class 'sim1D' from the output of a dynamic, 1D model.
#'
#' @param dyn Numeric matrix output by the ODE solver
#'   (e.g. \code{\link[deSolve]{ode}}) for a 1D model.
#' @param namesVars Names of the state variables in \code{dyn}. If \code{dyn}
#'   was produced with a \code{\link[rodeo]{rodeo}}-based model, the model
#'   object's \code{namesVars()} method returns the required names.
#' @param namesPros Names of the processes in \code{dyn}. If \code{dyn}
#'   was produced with a \code{\link[rodeo]{rodeo}}-based model, the model
#'   object's \code{namesPros()} method returns the required names.
#' @param xMin Minimum coordinates of the spatial boxes, e.g. top layer
#'   boundaries in a vertical 1D model.
#' @param xMax Maximum coordinates of the spatial boxes, e.g. bottom layer
#'   boundaries in a vertical 1D model.
#' @param keepItems Names of the items to be kept. Must be names of state
#'   variables or processes existing in the model.
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

sim1D.create <- function(dyn, namesVars, namesPros, xMin, xMax,
  keepItems=c(namesVars,namesPros), sep=".",
  rangeT=c(NA,NA), rangeX=c(NA,NA), nsig=2
) {
  if (length(xMin) != length(xMax))
    stop("'xMin' and 'xMax' differ in length")
  if (any(xMin > xMax))
    stop("detected case(s) where 'xMin' > 'xMax'")
  if ((length(xMin > 1)) &&
    any(abs(xMin[2:length(xMin)] - xMax[1:(length(xMax)-1)]) > .Machine$double.eps))
    stop("intervals defined by 'xMin' and 'xMax' not contiguous")
  if (!all(is.matrix(dyn), is.numeric(dyn)))
    stop("'dyn' should be a numeric matrix")
  if (colnames(dyn)[1] != "time")
    stop("expecting 'time' as first column of 'dyn'")
  if (!identical(colnames(dyn), c("time",
    paste(rep(namesVars, each=length(xMin)), 1:length(xMin), sep=sep),
    paste(rep(namesPros, each=length(xMin)), 1:length(xMin), sep=sep))))
    stop("column names of 'dyn' not matching expected values")
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
  res <- array(data=dyn, dim=c(length(times), length(whichX), length(keepItems)))
  attr(res, "times") <- times
  attr(res, "coordinates") <- cbind(min=xMin, max=xMax)
  attr(res, "items") <- keepItems
  class(res) <- "sim1D"
  return(res)
}

#' Query an Object of Class 'sim1D'
#'
#' Query an object of class 'sim1D' holding the output of a dynamic, 1D model.
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
#' @param attrib Logical. Controls whether the return values has additional
#'   attributes set (see below).
#'
#' @return A numeric matrix where rows represent time and columns represent
#'   boxes/layers. The matrix has neither row names nor column names, but
#'   if \code{attrib} is \code{TRUE}, the following attributes are set:
#' \itemize{
#'   \item{\code{times} : } Numeric vector of times corresponding to the rows.
#'   \item{\code{coordinates} : } Spatial coordinates corresponding to the
#'     columns. This is a matrix with two colums 'min' and 'max'.
#' }
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
#' sim1D.query(obj, item="X")

sim1D.query <- function(obj, item, rangeT=c(NA,NA), rangeX=c(NA,NA),
  partly=FALSE, attrib=TRUE)
{
  if (class(obj) != "sim1D")
    stop("'obj' is not an object of class 'sim1D'")
  if (!all(length(item) == 1, is.character(item)))
    stop("'item' should be a scalar character string")
  if (!all(length(rangeT) == 2, length(rangeX) == 2))
    stop("'rangeT' and 'rangeX' should be vectors of length 2")
  # Find item index
  items <- attr(obj, which="items", exact=TRUE)
  whichI <- which(items == item)
  if (length(whichI) != 1)
    stop("'obj' has no data for item '",item,"'")
  # Find time indices
  times <- attr(obj, which="times", exact=TRUE)
  rangeT[1] <- if (is.na(rangeT[1])) -Inf else min(as.numeric(rangeT))
  rangeT[2] <- if (is.na(rangeT[2])) Inf else max(as.numeric(rangeT))
  whichT <- which((times >= rangeT[1]) & (times <= rangeT[2]))
  if (length(whichT) < 1)
    stop("'obj' has no data for time range ",rangeT[1]," - ",rangeT[2])
  # Find spatial indices
  coord <- attr(obj, which="coordinates", exact=TRUE)
  rangeX[1] <- if (is.na(rangeX[1])) -Inf else min(as.numeric(rangeX))
  rangeX[2] <- if (is.na(rangeX[2])) Inf else max(as.numeric(rangeX))
  if (partly)
    whichX <- which((coord[,"max"] >= rangeX[1]) & (coord[,"min"] <= rangeX[2]))
  else
    whichX <- which((coord[,"min"] >= rangeX[1]) & (coord[,"max"] <= rangeX[2]))
  if (length(whichX) < 1)
    stop("'obj' has no data for x range ",rangeX[1]," - ",rangeX[2])
  # Return data matrix
  res <- obj[whichT,whichX,whichI]
  if (attrib) {
    attr(res, "times") <- times[whichT]
    attr(res, "coordinates") <- coord[whichX,]
  }
  return(res)
}

#' Final state of a 'sim1D' object 
#'
#' Extract data from object of Class 'sim1D' for the very last time step
#'
#' @param obj Object created with \code{sim1D.create}.
#'
#' @return A numeric matrix where rows represent boxes/layers. There is one
#'   column for each item and columns are named accordingly. Rows are unnamed
#'   but the following attributes are set:
#' \itemize{
#'   \item{\code{time} : } Scalar numeric representing the last time point for
#'     which data are stored in \code{obj}.
#'   \item{\code{coordinates} : } Spatial coordinates corresponding to the
#'     rows of the result. This is a matrix with two colums 'min' and 'max'.
#' }
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
#' sim1D.final(obj)

sim1D.final <- function(obj)
{
  if (class(obj) != "sim1D")
    stop("'obj' is not an object of class 'sim1D'")
  # Return data matrix
  res <- obj[dim(obj)[1],,]
  colnames(res) <- attr(obj, which="items", exact=TRUE)
  attr(res, "time") <- attr(obj, which="times", exact=TRUE)[dim(obj)[1]]
  attr(res, "coordinates") <- attr(obj, which="coordinates", exact=TRUE)
  return(res)
}


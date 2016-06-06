#' Assign colors to numeric values
#'
#' Associates numeric values with a set of colors.
#'
#' @param x Numeric vector.
#' @param breaks Break points for the colors. In the absence of more suitable
#'   information, quantiles of \code{x} are often appropriate.
#' @param colors Vector of colors corresponding to \code{breaks}.
#' @param lower If \code{TRUE}, the data in \code{breaks} are interpreted as
#'   lower boundaries of color intervals (thus the last element of \code{colors}
#'   is ignored unless \code{max(x)} equals \code{max(breaks)}). If \code{TRUE},
#'   the data in \code{breaks} are interpreted as upper interval boundaries
#'   (thus the first element of \code{colors} is ignored unless \code{min(x)}
#'   equals \code{min(breaks)}).
#' @param warn Warn if \code{x} contains values outside \code{range(breaks)}?
#' @param NAcolor Color assigned to \code{NA} values in \code{x}.
#'
#' @return A vector of colors of the same length as \code{x}.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#' x= runif(10)
#' x[3]= NA
#' x[5]= 2
#' breaks= c(0, .33, 0.66, 1)
#' colors= colorRampPalette(c("steelblue2","khaki2","brown"))(length(breaks))
#' low=FALSE
#' barplot(height=x, names.arg=1:length(x), ylim=c(0,1.2),
#'   col=colorQuant(x, breaks, colors, low))
#' abline(h=breaks, lty=3)
#' legend("top", bty="n", horiz=TRUE, fill=colorQuant(breaks, breaks, colors, low),
#'   legend=paste(ifelse(low, ">", "<"),breaks))

colorQuant= function(x, breaks, colors, lower=TRUE, warn=TRUE, NAcolor="transparent") {
  if (length(breaks) != length(colors))
    stop("'breaks' and 'colors' must be of equal length")
  if (((min(x, na.rm=TRUE) < min(breaks)) ||
    (max(x, na.rm=TRUE) > max(breaks))) && warn)
    warning("'x' value(s) outside range [",min(breaks), " , ", max(breaks),"]")  
  clr= data.frame(stringsAsFactors=FALSE, breaks=breaks, colors=colors)
  clr= clr[order(clr[,"breaks"]),]
  i= stats::approx(x=breaks, y=1:length(colors), xout=x, method="constant",
    f=if (lower) 0 else 1, rule=1)$y
  return(ifelse(is.na(i), NAcolor, colors[i]))
}



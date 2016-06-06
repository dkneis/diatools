#' Assign colors to nominal values
#'
#' Associates nominal values with a set of colors.
#'
#' @param x Vector of character strings.
#' @param classes Vector of character strings defining the class names.
#'   Typically, this would be \code{unique(x)}.
#' @param colors Vector of colors corresponding to \code{classes}.
#' @param warn Warn if \code{x} contains elements not present in \code{classes}?
#' @param NAcolor Color used for \code{x} values not present in \code{classes}.
#'
#' @return A vector of colors of the same length as \code{x}.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#' classes=c("A","B","C")
#' colors=c("red","blue","green")
#' x= runif(1000) * 100
#' y= runif(length(x)) * 100
#' z= rep("A", length(x))
#' z[x*y > 50] = "B"
#' z[x*y > 500] = "C"
#' z[x*y > 5000] = "D"
#' plot(x, y, col=colorQual(z, classes, colors, NAcolor="grey"))
#' legend("topright", bg="white", pch=1, col=colors, legend=classes)

# Function to assign colors to nominal values
colorQual= function(x, classes, colors, warn=TRUE, NAcolor="transparent") {
  if (length(classes) != length(colors))
    stop("'classes' and 'colors' must be of equal length")
  i= match(x, classes)
  if (any(is.na(i)) && warn)
    warning("assigning NAcolor to unknown class(es) '",
      paste(sort(unique(x[is.na(i)])), collapse="', '"),"'")
  return(ifelse(is.na(i), NAcolor, colors[i]))
}


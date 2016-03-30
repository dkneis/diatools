#' Advection velocities in porous medium
#'
#' Yields advection velocities of a solid or liquid component in a porous
#' medium to fulfil the constraint of mass conservation (continuity eqn.). 
#'
#' @param uRef Reference advection velocity (e.g. at a certain depth).
#' @param pRef Porosity corresponding to \code{uRef}.
#' @param p Vector of porosities for which advection velocities are to be returned.
#' @param solid Logical. Should velocities be returned for solids or liquid?
#'
#' @return Vector of the same length as \code{p}.
#'
#' @note The underlying principle of continuity is also known as steady-state
#'   compaction; see \code{ReacTran::setup.compaction.1D}.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#' z= 0:10
#' p= seq(0.99, 0.95, length.out=length(z))
#' plot(z, adVeloc(uRef=1, pRef=p[length(p)], p=p, solid=TRUE), type="l")
#' lines(z, adVeloc(uRef=1, pRef=p[length(p)], p=p, solid=FALSE), lty=2)
#' legend("top", bty="n", lty=1:2, legend=c("solid","liquid"))

adVeloc <- function (uRef, pRef, p, solid) {
  if (!solid) {
    return( uRef * pRef / p )
  } else {
    return( uRef * (1-pRef) / (1-p) )
  }
}



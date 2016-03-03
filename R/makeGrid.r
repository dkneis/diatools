#' Depth grid for 1D models
#'
#' Creates a grid with high resolution near surface and lower resolution at
#' greater depth.
#'
#' @param dz0 Resolution at surface.
#' @param dzMax Resolution at suggested maximum depth.
#' @param zMax Suggested maximum depth.
#' @param beta Shape parameter. The default of 1 creates a grid whose resolution
#'   declines linearly with depth.
#'
#' @return A data frame with 1 row per layer and the following columns
#' \itemize{
#'   \item {dz} Thickness of layer.
#'   \item {zUp} Depth of layer's upper boundary.
#'    \item {zLw} Depth of layer's lower boundary.
#' }
#'
#' @note The maximum depth of the generated grid (i.e. the lower boundary of the
#'  last layer) is guaranteed to be >= \code{zMax}.
#'  For a grid with high resolution near the surface but low resolution at
#'  greater depth one typically wants to set \code{beta} to a value > 1.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#' # 0.3 m total depth, resolution of 2 mm near surface and 2 cm at depth
#' gr= makeGrid(dz0=0.002, dzMax=0.02, zMax=0.3, beta=2)
#' plot(gr$zUp, gr$dz, type="S", xlab="Depth", ylab="Thickness of layer")

makeGrid= function (dz0, dzMax, zMax, beta=1) {
  dz= dz0
  while (sum(dz) < zMax) {
    dz= c(dz, dz0 + (dzMax - dz0) * (1 - (1 - sum(dz)/zMax)^beta))
  }
  return(data.frame(dz=dz, zLw=cumsum(dz), zUp=cumsum(dz)-dz))
}


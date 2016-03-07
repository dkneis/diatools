#' Uni-modal seasonality
#'
#' Approximation of seasonal variation by a (possibly asymmetric) sine function
#'
#' @param daynum Day of the year (value in range 1:366). Can be a vector.
#' @param daymin Day of the annual minimum, scalar.
#' @param daymax Day of the annual maximum, scalar.
#' @param ymin Value of the annual minimum, scalar.
#' @param ymax Value of the annual maximum, scalar.
#'
#' @return Numeric vector of the same length as \code{daynum}.
#'
#' @note A year is assumed to consist of 365 days. The result for day 366 is the
#'   same as for day 365. See the example for how this function can be used with
#'   values of class \code{POSIXct}.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#' # Lake temperature: min. of 3 degree C at day 20, max. 23 degree C at day 210
#' times= seq(from=ISOdatetime(2000,1,1,0,0,0),
#'   to=ISOdatetime(2002,1,1,0,0,0), by=3600)
#' dayofyear= as.integer(format(times, "%j"))
#' plot(times, seasonal(dayofyear, 20, 210, 3, 23), type="l", ylab="Temperature")

seasonal = function(daynum, daymin, daymax, ymin, ymax) {
  PI=3.1415; ONE=1; TWO=2; LASTDAY=365
  if (any((daynum < 1) | (daynum > 366)))
    stop("day number out of range")
  if ((length(daymin) != 1) | (length(daymax) != 1) |
    (length(ymin) != 1) | (length(ymax) != 1))
    stop("parameters of seasonality must be scalars")
  daynum[daynum==366]= 365
  d= ifelse(daynum >= daymin, daynum, LASTDAY + daynum)
  f= ifelse(d <= daymax, (d-daymin) / (daymax-daymin),
    ONE - (d-daymax)/(LASTDAY+daymin-daymax))
  return(ymin + (ymax - ymin) * (ONE + sin(-PI/TWO + f*PI)) / TWO)
}


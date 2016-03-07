#' Uni-modal seasonality
#'
#' Approximation of seasonal variation by a (possibly asymmetric) sine function
#'
#' @param daynum Day of the year (value in range 1:366). Can be a vector.
#' @param day1 First day with fixed value, scalar.
#' @param day2 Second day with fixed value, scalar. Must be > \code{day1}.
#' @param y1 Value at \code{day1}, scalar.
#' @param y2 Value at \code{day2}, scalar.
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

seasonal = function(daynum, day1, day2, y1, y2) {
  PI=3.1415; ONE=1; TWO=2; LASTDAY=365
  if (any((daynum < 1) | (daynum > 366)))
    stop("day number out of range")
  if ((length(day1) != 1) | (length(day2) != 1) |
    (length(y1) != 1) | (length(y2) != 1))
    stop("parameters of seasonality must be scalars")
  if (day2 <= day1)
    stop("expecting day2 > day1")
  daynum[daynum==366]= 365
  d= ifelse(daynum >= day1, daynum, LASTDAY + daynum)
  f= ifelse(d <= day2, (d-day1) / (day2-day1),
    ONE - (d-day2)/(LASTDAY+day1-day2))
  return(y1 + (y2 - y1) * (ONE + sin(-PI/TWO + f*PI)) / TWO)
}


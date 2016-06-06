#' Transforms a list into a more compact format
#'
#' Transforms a list of scalars, vectors, or matrices into a simplified
#' structure with one additional dimension (i.e. a vector, matrix, or 3D array).
#'
#' @param L List to be transformed. All elements of \code{L} must share the same
#'   type, length, dimension(s) and name(s). See the return value for supported
#'   element types.

#' @return The returned type depends on the type of the elements of \code{L}.
#' \itemize{
#'   \item{scalar case: } If each element of \code{L} is a scalar, the result
#'     is a vector of length \code{length(L)}.
#'   \item{vector case: } If each element of \code{L} is a vector, the result
#'     is a matrix. The number of rows equals \code{length(L)} and the number
#'     of columns is determined by the length of the vectors, i.e.
#'     \code{length(L[[1]])}.
#'   \item{matrix case: } If each element of \code{L} is a matrix, the result
#'     is an array with 3 dimensions. The length of dimension 1 equals the
#'     number of the matrices' rows, the length of dimension 2 equals the
#'     number of the matrices' columns, and the length of the 3rd dimension is
#'     \code{length(L)}.
#' }
#'
#' @note An error is generated if the restrictions with respect to the elements
#'   of \code{L} are not met.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#' # List of scalars
#' x= list(a=1, b=2, c=3)
#' print(simplifyList(x))  # same as unlist(x)
#' # List of vectors
#' x= list(a=1:2, b=2:3, c=3:4)
#' print(simplifyList(x))
#' # List of matrices
#' m= matrix(1:6, ncol=2)
#' colnames(m)= paste0("col",1:ncol(m))
#' rownames(m)= paste0("row",1:nrow(m))
#' x= list(a=m*1, b=m*2, c=m*3)
#' print(simplifyList(x))

simplifyList= function(L) {
  # All elements of the same simple type?
  type= unique(sapply(L, typeof))
  if (length(type) != 1) {
    stop("list elements differ in type ('",
      paste(type, collapse="', '"),"')")
  }
  types= c("logical", "integer", "double", "character")
  if (!type %in% types) {
    stop("list elements are of type '",type,
     "' but should be one of '",paste(type, collapse="', '"),"'")
  }
  # All outputs of the same length?
  len= unique(sapply(L, length))
  if (length(len) != 1) {
    stop("list elements differ in length")
  }
  # Comparison function
  same= function(x, y, fun) {identical(fun(x), fun(y))}
  # fn outputs are matrices
  if (all(sapply(L, is.matrix))) {
    if (!all(sapply(L, same, y=L[[1]], fun=ncol)))
      stop("list elements are matrices that differ in the number of columns")
    if (!all(sapply(L, same, y=L[[1]], fun=nrow)))
      stop("list elements are matrices that differ in the number of rows")
    if (!all(sapply(L, same, y=L[[1]], fun=colnames)))
      stop("list elements are matrices that differ in column names")
    if (!all(sapply(L, same, y=L[[1]], fun=rownames)))
      stop("list elements are matrices that differ in row names")
    L= array(unlist(L), dim=c(nrow(L[[1]]), ncol(L[[1]]), length(L)),
      dimnames=list(rownames(L[[1]]), colnames(L[[1]]), names(L)))
  # fn outputs are vectors
  } else if (all(sapply(L, is.vector)) && (len > 1)) {
    if (!all(sapply(L, same, y=L[[1]], fun=names)))
      stop("list elements are vectors that differ in element names")
    L= t(array(unlist(L), dim=c(len, length(L)),
      dimnames=list(names(L[[1]]), names(L))))
  # fn outputs are scalars
  } else if (all(sapply(L, is.vector)) && (len == 1)) {
    L= unlist(L)
  # fn outputs are something else
  } else {
    stop("list elements are of a type that isn't supported yet")
  }
}



# Turns of a warning during package building related to foreach %dopar%
utils::globalVariables("indexOfSet")

#' Sensitivity analysis
#'
#' Evaluates a function for multiple parameter sets, optionally using
#' parallel processing.
#'
#' @param p A numeric matrix representing the parameter sets to be tested.
#'   Each row represents a parameter set. Column names are interpreted as the
#'   names of the parameters.
#' @param fn Function of interest. It must accept as its first argument
#'   a named numeric vector of parameters. See \code{passIndex} for the meaning
#'   of a possible second argument. There are no restrictions with
#'   respect to the return value of \code{fn}.
#' @param n A positive integer specifying the number of child processes. It is
#'   passed as the first argument to \code{\link[parallel]{makeCluster}}.
#' @param passIndex Logical. If \code{TRUE}, function \code{fn} must accept as
#'   its second argument an integer value representing the index of the
#'   currently processed parameter set. 
#' @param silent Logical. Use this to suppress diagnostic messages.
#' @param logfile Name of a file to collect output messages of child processes.
#'   If this is an empty string (default) output messages are likely to appear
#'   on the screen.
#' @param ... Additional arguments passed to function \code{fn}.
#'
#' @return A list with the following components.
#' \itemize{
#'   \item{\code{nFailed} : } The number of 'failed' runs. These are runs where
#'     \code{fn} triggered a warning or an error.
#'   \item{\code{whichOK} : } A vector of length \code{nrow(p)} minus
#'     \code{nFailed} holding the numeric indices of the successful runs.
#'     It can be used to remove the 'abnormal' parameter sets from the input
#'     matrix \code{p}, e.g. \code{p[whichOK,]}.
#'   \item{\code{fnOut} : } A list where each element holds the return value of
#'     \code{fn} for a single parameter set. For example,
#'     \code{fnOut[i]} is the output from \code{fn(p[i,], i, ...)}. Elements
#'     where \code{fn} generated a warning or error are automatically dropped,
#'     thus, the list is of the same length as the vector \code{whichOK}.
#'     It will often be necessary to transform the list into a more handy data
#'     structure, e.g. using the \code{\link{simplifyList}} utility.
#'   \item{\code{cpu} : } A numeric vector holding the times spent on the
#'     successfull evaluations of \code{fn}. The vector has the same length as
#'     \code{whichOK}.
#' }
#'
#' @note The function of interest is called as \code{fn(p[i,], ...)} or
#'   \code{fn(p[i,], i, ...)}, depending on the value of \code{passIndex}. In
#'   the latter case, the index of the currently processed set is available within
#'   \code{fn}. This information can be used, for example, for diagnostic
#'   messages of for creating file names which are unique for each (possibly
#'   parallel) instance of \code{fn}.
#'
#'   In any case, \code{fn} is executed within a \code{\link[base]{tryCatch}}
#'   block. Any error messages (or warnings) are reported as warnings.
#'
#'   Any data or functions needed within \code{fn} should explicitly be passed
#'   using the \code{...} argument.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#' # Analysis of a model's goodness-of-fit
#' model= function(p, x) {
#'   if (p["b"] < 0)
#'     stop("negative argument for 'sqrt'") # to demonstrate handling of errors
#'   p["a"] * sqrt(x * p["b"])
#' }
#' # Observations
#' obs= cbind(x=1:50, y=model(c(a=1, b=0.1), 1:50))
#' # Objective function
#' mse= function(p, model, obs) { mean((obs[,"y"] - model(p, obs[,"x"]))^2) }
#' # Parameter sets to try
#' nSets= 10
#' p= cbind(a= seq(0, 2, length.out=nSets), b= seq(-0.2, 0.5, length.out=nSets))
#' # Evaluate obj. function for all sets
#' x= suppressWarnings(
#'   sensit(p=p, fn=mse, n=2, model=model, obs=obs, logfile=""))
#' # Show parameter sets that 'worked' together with results
#' print(cbind(setIndex=x$whichOK, p[x$whichOK,], mse=simplifyList(x$fnOut)))

sensit= function(p, fn, n=1, passIndex=FALSE, silent=FALSE, logfile="", ...) {

  # Check inputs
  if (!is.function(fn))
    stop(paste0("'fn' must be a function"))
  if (!all(c(is.matrix(p), is.numeric(p), !is.null(colnames(p)))))
    stop("'p' must be a numeric matrix having column names")

  # Create cluster
  if (n > 1) {
    cl <- parallel::makeCluster(n, outfile=logfile)
    doParallel::registerDoParallel(cl)
  }

  # Function to process a single set
  f= function(i, ...) {
    if (!silent)
      print(paste0("Set ",i," of ",nrow(p)))
    fnOut= NA
    cpu= NA
    tryCatch({
      t0= Sys.time()
      fnOut= if (passIndex) {
        fn(stats::setNames(p[i,],colnames(p)), i, ...)
      } else {
        fn(stats::setNames(p[i,],colnames(p)), ...)
      }
      t1= Sys.time()
      cpu= as.numeric(difftime(t1, t0, units="secs"))
    }, warning= function(w) {
      warning("warning issued by 'fn' for set ",i,". ",w)
    }, error= function(e) {
      warning("stop during evaluation of 'fn' for set ",i,". ",e)
    })
    return(list(fnOut=fnOut, cpu=cpu))
  }

  # Process all sets
  if (n > 1) {
    tmp= foreach::foreach(indexOfSet=1:nrow(p)) %dopar% f(indexOfSet, ...)
    parallel::stopCluster(cl)
  } else {
    tmp= f(1, ...)
  }

  # Split components of result
  cpu= unlist(lapply(tmp, function(x){x$cpu}))
  fnOut= lapply(tmp, function(x){x$fnOut})

  # Drop results for failed model runs
  ok= !is.na(cpu)
  cpu= cpu[ok]
  fnOut= fnOut[ok]
  if (length(fnOut) > 0)
    names(fnOut)= paste0("set",which(ok))

  # Return tested parameter values and function results
  return(list(nFailed=sum(!ok), whichOK=which(ok), fnOut=fnOut, cpu=cpu))
}


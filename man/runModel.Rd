% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/minimalisticInterface.r
\name{runModel}
\alias{runModel}
\title{Minimalistic generic model interface}
\usage{
runModel(model, input, outdir)
}
\arguments{
\item{model}{Function representing the model. It is called as
\code{model(input, outdir)}.}

\item{input}{List with model inputs. See below for expected contents.}

\item{outdir}{Name of an existing directory where any output files created
by \code{model} should be stored. Do \code{not} assume that \code{model}
cares/warns for existing files in \code{outdir}.}
}
\value{
The return value of \code{model}.
}
\description{
Wrapper function to run an arbitrarily complex model using a minimalistic
interface.
}
\note{
The following columns are mandatory in data frame \code{input}:
\itemize{
  \item{\code{name} : } Denotes an input item of the model. Typically it is a
                        parameter, a variable, or a dimension constant
                        (character string).
  \item{\code{value} : } Value of the input item (numeric).
  \item{\code{label} : } Short, human-readable label for the input item
                         (character string). Can be used, e.g., in graphics
                         produced by the model. Example: For a variable with
                         name 'NO3', the label could be 'Nitrate (mg/l)'.
}
}
\examples{
# This model does no computations, it only creates a html table of its inputs
input= list(tbl=data.frame(name=c("a","b"), value=1:2))
model= function(input, outdir) {
  html= paste0("<table border=1>\\n  <tr><th> ",paste(names(input$tbl),
    collapse=paste0(" </th><th> "))," </th></tr>\\n")
  for (i in 1:nrow(input$tbl)) {
    html= paste0(html,"  <tr>",paste0("<td style='text-align:right'> ",
      unlist(input$tbl[i,])," </td>",collapse=""),"</tr>\\n")
  }
  html= paste0(html,"</table>\\n")
  write(file=paste(outdir,"out.html",sep="/"), x=html)
  return(invisible(NULL))
}
outdir=tempdir()
runModel(model=model, input=input, outdir=outdir)
}
\author{
David Kneis \email{david.kneis@tu-dresden.de}
}


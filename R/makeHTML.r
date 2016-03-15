#' Arrange figures in HTML
#'
#' Embed a collection of figures in HTML with basic navigation capabilities
#'
#' @param x A data frame listing the graphics files to embed. See details below.
#' @param title Character string used as a title for the HTML page.
#'
#' @return HTML code as a character string. Needs to be printed to a file, e.g.
#'   using \code{write}.
#'
#' @note The data frame \code{x} must have 4 columns named 'h1', 'h2', 'width',
#'   and 'file' all being of character type. The first two columns specify
#'   headers of category 1 and 2, respectively. The 'h2' column can contain
#'   \code{NA} values where sub-headings are to be suppressed.
#'   The width is best specified as a percentage,
#'   e.g. '50%'. The last column contains the names of the graphics files to
#'   display. The files need to be in a format supported by the web browser
#'   such as SVG or PNG for vector and pixel graphics, respectively.
#'
#'   The data frame is \emph{not} sorted by any column (not even 'h1').
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#' test= data.frame(stringsAsFactors=FALSE,
#'   h1=c("summer","summer","winter","winter"),
#'   h2=c("TP","TN","TP","TN"),
#'   width="50%",
#'   file=c("summerTP.svg","summerTN.svg","winterTP.svg","winterTN.svg")
#' )

makeHTML = function(x, title="untitled") {
  cols= c("h1","h2","width","file")
  if (!all(cols %in% names(x)))
    stop("expected columns: '",paste(cols, collapse="', '"),"'")
  x$ref= paste0("ref",1:nrow(x))
  html="<html>"
  # Header
  html=paste0(html,"\n<head>","\n  <title>",title,"</title>","\n  <style>",
    "
    #wrapper{min-height:100%; width:100%; position:relative;
      background-color:White; display:inline-block;}
    #sidebar{width:15%; top:0px; left:0px; bottom:0px; position:fixed;
      background-color:#E4E4E4; overflow:scroll;}
    #content{min-height:500px; width:80%; position:relative;
      background-color:White; float:right; overflow-x:scroll;}
    h1{color:#5075A9; line-height:20px; margin-top:40px; margin-bottom:2px;}
    h2{color:#5075A9; line-height:14px; margin-top:25px; margin-bottom:2px;}
  ",
  "</style>\n</head>")
  # Open body and wrapper
  html=paste0(html,"\n<body>","\n  <div id=\"wrapper\">")
  # Side bar
  html=paste0(html,"\n    <div id=\"sidebar\">")
  for (i in 1:nrow(x)) {
    if ((i == 1) || (!identical(x$h1[i],x$h1[i-1])))
      html= paste0(html,"\n      <b><a href=\"#",x$ref[i],"\">",x$h1[i],"</a></b><p>")
    if (!is.na(x$h2[i])) {
      if ((i == 1) || (!identical(x$h2[i],x$h2[i-1])))
        html= paste0(html,"\n      <ul><li><a href=\"#",x$ref[i],"\">",x$h2[i],"</a></li></ul><p>")
    }
  }
  html=paste0(html,"\n    </div>")
  # Contents
  html=paste0(html,"\n    <div id=\"content\">")
  for (i in 1:nrow(x)) {
    if ((i == 1) || (!identical(x$h1[i],x$h1[i-1])))
      html= paste0(html,"\n      <h1><a name=\"",x$ref[i],"\">",x$h1[i],"</a></h1><p>")
    if (!is.na(x$h2[i])) {
      if ((i == 1) || (!identical(x$h2[i],x$h2[i-1])))
        html= paste0(html,"\n      <h2><a name=\"",x$ref[i],"\">",x$h2[i],"</a></h2><p>")
    }
    html= paste0(html,"\n      <img src=\"",x$file[i],"\" width=\"",x$width[i],
      "\" height=\"auto\" alt=\"",x$file[i],"\">")
  }
  html=paste0(html,"\n    </div>")
  # Close wrapper and body
  html=paste0(html,"\n  </div>","\n</body>")
  html=paste0(html,"</html>")
}


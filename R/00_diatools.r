#' Utilities for diagenetic modeling
#'
#' Utilities for diagenetic modeling
#'
#' @name diatools-package
#' @aliases diatools
#' @docType package
{}

################################################################################
# Required packages

# Package for plotting of color maps with legend
#' @import fields
if (!("fields" %in% installed.packages()[,1]))
  install.packages("fields")
library("fields")


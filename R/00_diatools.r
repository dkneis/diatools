#' Utilities for Diagenetic Modeling
#'
#' A small collection of utilities for diagenetic modeling
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

# Packages for parallel processing
#' @import foreach
if (!("foreach" %in% installed.packages()[,1]))
  install.packages("foreach")
library("foreach")
#' @import doParallel
if (!("doParallel" %in% installed.packages()[,1]))
  install.packages("doParallel")
library("doParallel")


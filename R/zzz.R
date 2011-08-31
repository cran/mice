#  zzz.R
#
#	 R package MICE: Multivariate Imputation by Chained Equations
#    Copyright (c) 1999-2010 TNO Quality of Life, Leiden
#	
#	 This file is part of the R package MICE.
#
# System functions for the MICE library

#------------------------------.onLoad-------------------------------
.onLoad <- function(...){
  require(MASS, quietly=TRUE)
  require(nnet, quietly=TRUE)
  require(lattice, quietly=TRUE)
  require(stringr, quietly=TRUE)

  d <- packageDescription("mice")
  packageStartupMessage(paste(d$Package,d$Version,d$Date))
  return()
}

version <- function(pkg="mice"){
  lib <- dirname(system.file(package = pkg))
  d <- packageDescription(pkg)
  return(paste(d$Package,d$Version,d$Date,lib))
}

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
  lib <- dirname(system.file(package = "mice"))
  d <- packageDescription("mice", lib.loc = lib)
  cat(paste(d$Package,d$Version,d$Date,"\n"))

  require(MASS, quietly=TRUE)
  require(nnet, quietly=TRUE)
  require(lattice, quietly=TRUE)
}

version <- function(pkg="mice"){
  lib <- dirname(system.file(package = pkg))
  d <- packageDescription(pkg, lib.loc = lib)
  return(paste(d$Package,d$Version,d$Date,lib))
}

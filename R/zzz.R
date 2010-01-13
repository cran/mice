#  zzz.R
#
#	 R package MICE: Multivariate Imputation by Chained Equations
#    Copyright (c) 1999-2010 TNO Quality of Life, Leiden
#	
#	 This file is part of the R package MICE.
#
# System functions for the MICE library V2.2

#------------------------------.First.lib-------------------------------
.First.lib <- function(library, section){
    cat("mice V2.2\n")
    # require(Hmisc)
    require(MASS)
    require(nnet)
}

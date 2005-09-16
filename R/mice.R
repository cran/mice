#	R package MICE: Multivariate Imputation by Chained Equations
#    Copyright (c) 2001, 2004, 2005 TNO Prevention and Health, Leiden
#	
#	This file is part of the R package MICE.
#
#    MICE is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    MICE is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with MICE; if not, write to the Free Software
#    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

# NOTE: FROM v1.14 all changes are logged in file /ChangeLog 
# 
# file last modified 26 April 05 by J. Fox
#  o  to insure generic/method consistency
#  o  to replace F with FALSE, T with TRUE
# 
# MICE: Multivariate Imputation by Chained Equations
#   in S-Plus 6.1 and R 1.8
#   
#   Authors:    K. Oudshoorn and S. van Buuren 
#           TNO Prevention and Health, Leiden
#           The Netherlands
#
#   See S. van Buuren & K. Oudshoorn (2000) MICE User's Guide V1.0,     
#   Leiden: TNO Prevention and Health. 
#
#
#   Revision history: 
#   V1.0    Original S-Plus release, June 14 2000
#   V1.1    R version (with help of Peter Malewski and Frank Harrell), Feb 2001
#   V1.12   Maintainance, S-Plus 6.1 and R 1.8 unicode, January 2004
#   V1.13   Changed function checkImputationMethod, Feb 6, 2004
#
#   Last change: January 13, 2004 SvB
#   ## SVB: changes by Stef van Buuren
#   ## FEH: changes by Frank E. Harrell 
#   ## PM:  changes by Peter Malewski
#   ## RJ:  changes by Roel de Jong
# 
#   Please report any bugs to dejongroel@gmail.com
#   See www.multiple-imputation.com for any updates.
#


# Define settings

.R.          <- length(version$language) && (version$language=='R')
.SV4.        <- !.R. && (version$major > 4)

##For compatibility with pre-version 5.x:
# Still need to source in definitions if S-Plus pre V6
# as may be compiling dist on another machine with S+2000 Rel 3
w <- (!.R.) && (!.SV4.)
if(w || !exists('oldUnclass'))  oldUnclass  <- unclass
if(w || !exists('oldClass'))    oldClass    <- class
if(w || !exists('oldClass<-'))
  'oldClass<-' <- function(x, value) {
    oldClass(x) <- value
    x
  }
if(w || !exists('logb'))        logb        <- log

if(w || !exists('existsFunction')) existsFunction <- function(...)
  exists(..., mode='function')
rm(w)


#---------------------- MICE ------------------------------------------------

mice<-function(data, 
    m = 5,
    imputationMethod = vector("character",length=ncol(data)), 
    predictorMatrix = (1 - diag(1, ncol(data))),
    visitSequence = (1:ncol(data))[apply(is.na(data),2,any)], 
    defaultImputationMethod=c("pmm","logreg","polyreg"),
    maxit = 5,																											
    diagnostics = TRUE, 
    printFlag = TRUE,
    seed = NA)

{
#   MICE - Multivariate Imputation by Chained Equations V1.12
#
#   Main algorithm for imputing datasets.
#   Authors:    K. Oudshoorn and S. van Buuren 
#           TNO Prevention and Health, Leiden
#           The Netherlands
#
#   See S. van Buuren & K. Oudshoorn (2000) MICE User's Guide V1.0,     
#   Leiden: TNO Prevention and Health. 
#
#   Copyright (c) 2002 TNO Prevention and Health, Leiden
#
#   Last change: April 21, 2002 SvB
#   ## FEH: changes by Frank E. Harrell 
#   ## PM:  changes by Peter Malewski
#
#  Start with some preliminary calculations and error checks
    call <- match.call()
    if(!is.na(seed)) set.seed(seed)  ## FEH 1apr02
    if(!(is.matrix(data) | is.data.frame(data))) stop("Data should be a matrix or dataframe")
    if ((nvar <- ncol(data)) < 2) stop ("Data should contain at least two columns")
    data <- as.data.frame(data)  
    nmis <- apply(is.na(data),2,sum)
    if (sum(nmis)==0) stop("No missing values found")
    varnames <- dimnames(data)[[2]]
    
    #
    #  If a column consists of entirely NA's, don't let it be a factor.
    #
    # data[,nmis==nrow(data)] <- as.numeric(NA)
    # the above statement creates problems if data is a matrix (28/1/00)
    #
    #   Perform various validity checks on the specified arguments
    #
    check.visitSequence(visitSequence, nmis, nvar)
    predictorMatrix <- check.predictorMatrix(predictorMatrix, nmis, nvar) 
    imputationMethod <- check.imputationMethod(imputationMethod, defaultImputationMethod, visitSequence, data, nmis, nvar)

    #
    #   Pad the imputation model with dummy variables for the factors
    #   
    p <- padModel(data, imputationMethod, predictorMatrix, visitSequence, nmis, nvar)
    if(sum(duplicated(names(p$data))) > 0) stop("Column names of padded data should be unique")   

    #
    #   Initialize response matrix r, imputation array imp, as well as some other stuff
    #
    r <- (!is.na(p$data))
    imp <- vector("list", ncol(p$data))
    if(m > 0) {

        #
        # Initializes the imputed values before entering the main 
        # iteration loop.
        #
        for(j in visitSequence) {
            imp[[j]] <- as.data.frame(matrix(0, nrow = sum(!r[,j]), ncol = m))
            dimnames(imp[[j]]) <- list(row.names(data)[r[,j] == FALSE], 1:m)        
            y <- data[,j]
            ry <- r[,j]
            for(i in 1:m) {
                if (nmis[j]<nrow(data)-1) {
                    # if (is.passive(imputationMethod[j])) p$data[ry,j] <- data[ry,j] <- model.frame(imputationMethod[j],data[ry,])
                    imp[[j]][,i] <- impute.sample(y, ry)
                }
                else imp[[j]][,i] <- rnorm(nrow(data))
            }
        }
    }
    
    q <- sampler(p, data, m, imp, r, visitSequence, maxit, printFlag)

    # restore the original NA's in the data
    for(j in p$visitSequence) p$data[(!r[,j]),j] <- NA

    # delete data and imputations of automatic dummy variables
    imp <- q$imp[1:nvar]
    names(imp) <- varnames
    dimnames(predictorMatrix) <- list(varnames,varnames)
    names(imputationMethod) <- varnames
    names(visitSequence) <- varnames[visitSequence]

    # save, and return  
    midsobj <- list(call = call, data = as.data.frame(p$data[,1:nvar]), 
            m = m, nmis = nmis, imp = imp, 
            imputationMethod = imputationMethod,
            predictorMatrix = predictorMatrix, 
            visitSequence = visitSequence, 
            seed = seed, 
            iteration = q$iteration, 
            lastSeedValue = .Random.seed,
            chainMean = q$chainMean, chainVar = q$chainVar) 
    if (diagnostics) midsobj <- c(midsobj, list(pad = p))
    oldClass(midsobj) <- "mids"
    return(midsobj)
} # END OF MICE FUNCTION

#---------------------- MICE.MIDS -------------------------------------------

mice.mids<-function(obj, maxit=1, diagnostics = TRUE, printFlag = TRUE)
{
# MICE.MIDS - 
# MICE algorithm that takes mids object as input, iterates maxit 
# iteration and produces another mids object as output.
    if (!is.mids(obj)) stop("Object should be of type mids.")
    if (maxit < 1) return(obj)
#
    call <- match.call()
    nvar <- ncol(obj$data)
    sumIt <- obj$iteration + maxit
    varnames <- dimnames(obj$data)[[2]]
#
#
    if (is.null(obj$pad)) p <- padModel(obj$data, obj$imputationMethod, obj$predictorMatrix, obj$visitSequence, obj$nmis, nvar)
    else p <- obj$pad
    r <- (!is.na(p$data))
    imp <- vector("list", ncol(p$data))
    for(j in obj$visitSequence) imp[[j]] <- obj$imp[[j]]
    assign(".Random.seed", obj$lastSeedValue, pos=1) ##pm 04/02
#
#
    q <- sampler(p, obj$data, obj$m, imp, r, obj$visitSequence, maxit, printFlag)

    # restore the original NA's in the data
    for(j in p$visitSequence) p$data[(!r[,j]),j] <- NA

    #   delete data and imputations of automatic dummy variables
    data <- p$data[,1:nvar]
    imp <- q$imp[1:nvar]
    names(imp) <- varnames

    # combine with previous chainMean and chainVar
    chainMean <- chainVar <- array(0, dim=c(length(obj$visitSequence),sumIt,obj$m),dimnames=list(varnames[obj$visitSequence],1:sumIt,paste("Chain",1:obj$m)))
    for(j in 1:length(obj$visitSequence)) {
        if (!is.factor(obj$data[,obj$visitSequence[j]])){
            if (obj$iteration==0) {
                chainMean[j,,] <- q$chainMean[j,,]
                chainVar[j,,] <- q$chainVar[j,,]
            } else {
                chainMean[j,,] <- rbind(obj$chainMean[j,,], q$chainMean[j,,])   
                chainVar[j,,] <- rbind(obj$chainVar[j,,], q$chainVar[j,,])
            }
        }
    }

    # save, and return  
    midsobj <- list(call = call, data = as.data.frame(data), 
            m = obj$m, nmis = obj$nmis, imp = imp, 
            imputationMethod = obj$imputationMethod,
            predictorMatrix = obj$predictorMatrix, 
            visitSequence = obj$visitSequence, 
            seed = obj$seed, 
            iteration = sumIt, 
            lastSeedValue = .Random.seed,
            chainMean = chainMean, chainVar = chainVar) 
    if (diagnostics) midsobj <- c(midsobj, list(pad = p))
    oldClass(midsobj) <- "mids"
    return(midsobj)
} # END OF MICE.MIDS FUNCTION

#------------------------------CHECK.VISITSEQUENCE-------------------------------
check.visitSequence<-function(visitSequence, nmis, nvar)
{
#   checks the visitSequence array
#  stops the program if an error is found
#
    # if (length(unique(visitSequence))!=length(visitSequence)) stop("Indices in visitSequence are not unique")
    if(all(nmis[visitSequence] == 0)) stop(paste("No missing values present in ", expression(data)))
    flags <- nmis==0 & is.element(1:nvar,visitSequence)
    if (any(flags)) stop(paste("Columns ",paste((1:nvar)[flags],collapse=1,sep=",")," requested to be imputed, but contain no missing values."))
}

#------------------------------CHECK.predictorMatrix-------------------------------
check.predictorMatrix<-function(predictorMatrix, nmis, nvar)
{
#   checks the predictorMatrix
#  it can change the predictormatrix
#  stops the program if an error is found
#
    if(!is.matrix(predictorMatrix)) stop("Argument predictorMatrix should be a square matrix.")
    if(nvar != nrow(predictorMatrix) | nvar != ncol(predictorMatrix)) stop("Argument predictorMatrix has improper dimensions.")
    if(sum(diag(predictorMatrix) != 0)) stop("The diagonal of predictorMatrix may contain only zeroes.")
    for(j in 1:nvar) {
        if(!any(predictorMatrix[j,]) & any(predictorMatrix[,j]) & nmis[j]>0) stop(paste("Column ", j, " is used, has missing values, but is not imputed")) 
        if(nmis[j]==0 & any(predictorMatrix[j,])) {
            # warning(paste("Row ",j," of predictorMatrix is set to zero"))
            predictorMatrix[j,] <- rep(0, nvar)
        }
    }
    return(predictorMatrix)
}

#------------------------------CHECK.imputationMethod-------------------------------

check.imputationMethod<-function(imputationMethod, defaultImputationMethod, visitSequence, data, nmis, nvar)
{
#
#   check imputationMethod, set defaults if appropriate
#  stops the program if an error is found
#
    if(all(imputationMethod == "")) { # the default argument
        for(j in visitSequence) {
            if(length(unique(data[,j])) == 2) stop(paste("Column ",j," is constant")) # NA is also a unique level
            else if(is.numeric(data[,j])) imputationMethod[j] <- defaultImputationMethod[1]
            else if(length(levels(data[,j])) == 2)  imputationMethod[j] <- defaultImputationMethod[2]
            else if(length(levels(data[,j])) > 2) imputationMethod[j] <- defaultImputationMethod[3]
        }
    }
#
#   expand user's imputation method to all visited columns
#
    if(length(imputationMethod) == 1) {# single string supplied by user
        if(is.passive(imputationMethod)) stop("Cannot have a passive imputationMethod for every column.")
        imputationMethod <- rep(imputationMethod, nvar)
    }
#
#  if user specifies multiple methods, check the length of the argument
#
    if(length(imputationMethod)!=nvar) stop(paste("The number of elements in imputationMethod (",length(imputationMethod),") does not match the number of columns (",nvar,")."))
#
#   check whether the elementary imputation methods are actually available on the search path
#
    active <- !is.passive(imputationMethod) & nmis > 0
    fullNames <- paste("impute", imputationMethod[active], sep=".")
    notFound <- !sapply(fullNames,exists,mode="function") ## SVB 6 Feb 2004
    # notFound <- !sapply(fullNames, existsFunction)  ## FEH
    if (any(notFound)) stop(paste("The following functions were not found:",paste(fullNames[notFound],collapse=", ")))
#
#
    return(imputationMethod)
}


#------------------------------PadModel-------------------------------

padModel <- function(data,  imputationMethod, predictorMatrix, visitSequence, nmis, nvar)
{
#   Called by mice().
#   Augments the imputation model by including dummy variables. Adapts data, predictorMatrix,
#  imputationMethod and visitSequence.
#  Returns a list whose components make up the padded model.
#   
    categories<-data.frame(yes.no.categorical=factor(rep(FALSE,nvar),levels=c("TRUE","FALSE")),number.of.dummies=rep(0,nvar),yes.no.dummy=factor(rep(FALSE,nvar),levels=c("TRUE","FALSE")),corresponding.column.dummy=rep(0,nvar))

    # zero is default in corresponding.column.dummy for no dummy variable
    for(j in 1:nvar) {
        if(is.factor(data[,j]) && any(predictorMatrix[,j])) {
            categories[j,1]<-TRUE
            data[,j]<-C(data[,j],treatment) #assign contrast-attribute, choice treatment, to factor
            n.dummy <- length(levels(data[,j])) - 1
            categories[j,2]<-n.dummy
            predictorMatrix <- rbind(predictorMatrix, matrix(0, ncol = ncol(predictorMatrix), nrow = n.dummy))
            predictorMatrix <- cbind(predictorMatrix, matrix(rep(predictorMatrix[,j],times=n.dummy), ncol = n.dummy))   
            predictorMatrix[1:nvar,j] <- rep(0, times = nvar)
            if (any(predictorMatrix[j,])){
                predictorMatrix[(ncol(predictorMatrix)-n.dummy+1):ncol(predictorMatrix),j]<-rep(1,times=n.dummy)
                #in predictorMatrix only FALSE/TRUE is allowed due to other part program.
                visitSequence<-append(visitSequence,ncol(predictorMatrix)-n.dummy+1,(1:length(visitSequence))[visitSequence==j])
            }
            data<-(cbind(data,matrix(0,ncol=n.dummy,nrow=nrow(data))))
            data[is.na(data[,j]),(ncol(predictorMatrix)-n.dummy+1):ncol(predictorMatrix)]<-NA
            ## PM
            if(.R.) cat.column <- data[!is.na(data[, j]), j] else
              assign("cat.column",data[!is.na(data[,j]),j],where=1)
            data[!is.na(data[,j]),(ncol(predictorMatrix)-n.dummy+1):ncol(predictorMatrix)]<-model.matrix(~cat.column-1)[,-1]
            names(data)[(ncol(predictorMatrix)-n.dummy+1):ncol(predictorMatrix)]<-paste(attr(data,"names")[j],"d",(1:n.dummy),sep=".")
            if(!.R.) rm(cat.column) ## FEH
            imputationMethod <- c(imputationMethod, rep("dummy", n.dummy))
            categories<-rbind(categories,data.frame(yes.no.categorical=rep(FALSE,n.dummy),
               number.of.dummies=rep(0,n.dummy),yes.no.dummy=rep(TRUE,n.dummy),corresponding.column.dummy=rep(j,n.dummy)))
        }
    }
    varnames <- dimnames(data)[[2]]
    dimnames(predictorMatrix) <- list(varnames,varnames)
    names(imputationMethod) <- varnames
    names(visitSequence) <- varnames[visitSequence]
    dimnames(categories)[[1]] <- dimnames(data)[[2]] 
    return(list(data=as.data.frame(data),predictorMatrix=predictorMatrix,imputationMethod=imputationMethod,visitSequence=visitSequence,categories=categories))
}


#------------------------------sampler-------------------------------

sampler <- function(p, data, m, imp, r, visitSequence, maxit, printFlag)
#
#   The sampler controls the actual Gibbs sampling iteration scheme
#   This function is called by mice and mice.mids
#
#   Authors: S van Buuren, K Oudshoorn
#   Copyright (c) 2000 TNO Prevention and Health
{
    # set up array for convergence checking
    if (maxit>0) chainVar <- chainMean <- array(0, dim=c(length(visitSequence),maxit,m),dimnames=list(dimnames(data)[[2]][visitSequence],1:maxit,paste("Chain",1:m)))
    else chainVar <- chainMean <- NULL

    # THE ITERATION MAIN LOOP: GIBBS SAMPLER
    if (maxit<1) iteration <- 0
    else {
    if (printFlag) cat("\n iter imp variable")
    for(k in 1:maxit) {  #begin k loop : iteration loop
        iteration <- k
        for(i in 1:m) { #begin i loop    : repeated imputation loop
            if (printFlag) cat("\n ",iteration," ",i)
            
            # fill the data with the last set of imputations
            for (j in visitSequence) p$data[!r[,j],j] <- imp[[j]][,i]

            # augment the data with the actual dummy variables
            for (j in setdiff(p$visitSequence,visitSequence)){
              if(.R.) cat.columns <- p$data[, p$categories[j, 4]] else
               assign("cat.columns",p$data[,p$categories[j,4]],where=1)
                p$data[,(j:(j+p$categories[p$categories[j,4],2]-1))] <- matrix((model.matrix(~cat.columns-1)[,-1]),ncol=p$categories[p$categories[j,4],2],nrow=nrow(p$data))
                if(!.R.) remove("cat.columns")  ## FEH
            }
            #
            # iterate once over the variables of the augmented model
            #
            for(j in p$visitSequence) {
                if (printFlag) cat(" ",dimnames(p$data)[[2]][j])
                theMethod <- p$imputationMethod[j]
                if((!is.passive(theMethod)) & theMethod!="dummy"){  # for a true imputation method
                    if (substring(theMethod, 1, 3) != "ml.") {			# RJ: for an non-multilevel imputation method 
                    	imp[[j]][,i] <- do.call(paste("impute",theMethod,sep="."), 
                        	args = list(p$data[,j], r[,j], p$data[,p$predictorMatrix[j,]==1,drop=FALSE]))
                    }
                    else { # for a multilevel imputation method
                    	predictors = p$predictorMatrix[j,] > 0
                    	imp[[j]][,i] <- do.call(paste("impute",theMethod,sep="."), 
                        	args = list(p$data[,j], r[,j], p$data[,predictors,drop=FALSE], p$predictorMatrix[j,predictors]))
                    }	    	
                    p$data[!r[,j],j] <- imp[[j]][,i]
                }
                else if (is.passive(theMethod)) {
                    imp[[j]][,i] <- model.frame(as.formula(theMethod), p$data[!r[,j],])	#RJ - FIXED passive imputation: as.formula()
                    p$data[!r[,j],j] <- imp[[j]][,i]
                }
                else if (theMethod== "dummy") {
                  ## FEH
                  if(.R.) cat.columns <- p$data[,p$categories[j,4]] else
                    assign("cat.columns",p$data[,p$categories[j,4]],where=1)
                    p$data[,(j:(j+p$categories[p$categories[j,4],2]-1))]<-
                             matrix((model.matrix(~cat.columns-1)[,-1]),ncol=p$categories[p$categories[j,4],2],nrow=nrow(p$data))
                    remove("cat.columns")
                }
           } # end j loop
        }   # end i loop
        for(j in 1:length(visitSequence)) {
            if (!is.factor(data[,visitSequence[j]])){
              if(.R.) {  ## PM
                chainVar[j, k, ] <- apply(imp[[visitSequence[j]]],2,var) 
                chainMean[j, k, ] <- colMeans(as.matrix(imp[[visitSequence[j]]])) ##pm 04/02
              } else {
                chainVar[j, k,] <- colVars(imp[[visitSequence[j]]])             
                chainMean[j, k,] <- colMeans(as.matrix(imp[[visitSequence[j]]]))
              }
              }
        }
    } # end iteration loop
    if (printFlag) cat("\n")
}
    return(list(iteration=iteration, imp=imp, chainMean = chainMean, chainVar = chainVar))
}




#
# impute.ssc
#
# Standard collection of elementary imputation functions.
# Part of Oudshoorn & Van Buuren MICE library V1.12
#
#----------------------------------IMPUTE.NORM----------------------------------

impute.norm<-function(y, ry, x)
{
# Bayesian normal imputation of y given x according to Rubin p. 167
# x is complete.
#
# TNO Prevention and Health
# authors: S. van Buuren and K. Oudshoorn
#
    x <- cbind(1, as.matrix(x))
    parm <- .norm.draw(y, ry, x)
    return(x[!ry,  ] %*% parm$beta + rnorm(sum(!ry)) * parm$sigma)
}

#----------------------------------------.NORM.DRAW-----------------------------
.norm.draw<-function(y, ry, x)
{
# .norm.draw
# Draws values of beta and sigma for Bayesian linear regrssion imputation 
# of y given x according to Rubin p. 167
# x is complete.
#
# TNO Prevention and Health
# authors: S. van Buuren and K. Oudshoorn
#
# adapted 17/12 nrow(x) should be sum(ry)
    xobs <- x[ry,  ]
    yobs <- y[ry]
    v <- solve(t(xobs) %*% xobs)
    coef <- t(yobs %*% xobs %*% v)
    residuals <- yobs - xobs %*% coef
    sigma.star <- sqrt(sum((residuals)^2)/rgamma(1, sum(ry) - ncol(x)))
    beta.star <- coef + (t(chol((v + t(v))/2)) %*% rnorm(ncol(x))) * sigma.star
    parm <- list(beta.star, sigma.star)
    names(parm) <- c("beta", "sigma")
    return(parm)
}

#----------------------------------IMPUTE.NORM.IMPROPER-------------------------
impute.norm.improper<-function(y, ry, x)
{
# impute.norm.improper
# Regression imputation of y given x, with a fixed regression
# line, and with random draws of the residuals around the line.
# x is complete.
#
# TNO Prevention and Health
# authors: S. van Buuren and K. Oudshoorn
#
#    x <- cbind(1, as.matrix(x))
#    parm <- .norm.fix(y, ry, x)
#    return(x[!ry,  ] %*% parm$beta + rnorm(sum(!ry)) * parm$sigma)
	stop("impute.norm.improper is disabled this release")
}



#-----------------------------IMPUTE.PMM-----------------------------------------------

impute.pmm<-function(y, ry, x)
{
# Imputation of y by predictive mean matching, based on
# Rubin (p. 168, formulas a and b).
# The procedure is as follows:
# 1. Draw beta and sigma from the proper posterior
# 2. Compute predicted values for yobs and ymis
# 3. For each ymis, find the observation with closest predicted value, 
#    and take its observed y as the imputation.
# NOTE: The matching is on yhat, NOT on y, which deviates from formula b.
# ry=TRUE if y observed, ry=FALSE if y missing
#
# Authors: S. van Buuren and K. Oudshoorn
# 2 dec SvB
#
    x <- cbind(1, as.matrix(x))
    parm <- .norm.draw(y, ry, x)
    yhat <- x %*% parm$beta
    return(apply(as.array(yhat[!ry]),1,.pmm.match,yhat=yhat[ry],y=y[ry]))
}

#-------------------------.PMM.MATCH---------------------------------
.pmm.match<-function(z, yhat=yhat, y=y)
{
# Auxilary function for impute.pmm.
# z    = target predictive value (scalar)
# yobs = array of yhat, to be matched against z
# y    = array of donor data values, same length as yobs.
# Returns the donor value for which abs(z-yobs) is minimal.
# In case of multiple matches, a random draw is made.
    d <- abs(yhat-z)
    m <- y[d==min(d)]
    if (length(m)>1) m <- sample(m,1)
    return(m)
}


#-------------------------IMPUTE.LOGREG------------------------------

impute.logreg<-function(y, ry, x)
{
# impute.logreg 
#
# Imputation for binary response variables by the Bayesian 
# logistic regression model. See Rubin (1987, p. 169-170) for
# a description of the method.
#
# The method consists of the following steps:
# 1. Fit a logit, and find (bhat, V(bhat))
# 2. Draw BETA from N(bhat, V(bhat))
# 3. Compute predicted scores for m.d., i.e. logit-1(X BETA)
# 4. Compare the score to a random (0,1) deviate, and impute.
#
# Authors: Stef van Buuren, Karin Oudshoorn, 1998
#
#
    x <- cbind(1, as.matrix(x))
    ## PM
    fit <- if(.R.)
      glm.fit(x[ry, ], y[ry], family = binomial(link = logit),
              control=glm.control(maxit = 10, trace = FALSE, epsilon =
                0.1)) else
    glm.fit(x[ry, ], y[ry], family = binomial(link = logit),
            maxit = 10, epsilon = 0.1, trace = FALSE)
    fit.sum <- summary.glm(fit)
    beta <- coef(fit)
    rv <- t(chol(fit.sum$cov.unscaled))
    beta.star <- beta + rv %*% rnorm(ncol(rv))  
    p <- 1/(1 + exp( - (x[!ry,  ] %*% beta.star)))
    vec <- (runif(nrow(p)) <= p)
    vec[vec] <- 1
    if (is.factor(y)) {
        vec<-factor(vec,c(0,1),levels(y))}
    return(vec)
}

#-------------------------IMPUTE.LOGREG2------------------------------

impute.logreg2 <- function(y, ry, x)
{
# impute.logreg2 
#
# Imputation for binary response variables by the Bayesian 
# logistic regression model. See Rubin (1987, p. 169-170) for
# a description of the method.
#
# This routine uses direct minimization of the likelihood.
#
# The method consists of the following steps:
# 1. Fit a logit, and find (bhat, V(bhat))
# 2. Draw BETA from N(bhat, V(bhat))
# 3. Compute predicted scores for m.d., i.e. logit-1(X BETA)
# 4. Compare the score to a random (0,1) deviate, and impute.
#
# Authors: Stef van Buuren, Karin Oudshoorn, 1998
#
#
#    x <- cbind(1, as.matrix(x))
#    dimnames(x)[[2]][1] <- "(Intercept)"
#    xobs <- x[ry,]
#    yobs <- as.vector(as.numeric(y[ry]))
#    #names(y) <- "y"
#    fit1 <- logitreg(xobs, yobs, intercept=FALSE)
#    p <- 1/(1 + exp( - (x[ry,  ] %*% coef(fit1))))
#    fit <- glm.fit(x[ry,], y[ry], family = binomial(link = logit), start = p, maxit = 1, trace = FALSE)
#    fit.sum <- summary.glm(fit)
#    beta <- coef(fit)
#    rv <- t(chol(fit.sum$cov.unscaled))
#    beta.star <- beta + rv %*% rnorm(ncol(rv))  
#    p <- 1/(1 + exp( - (x[!ry,  ] %*% beta.star)))
#    vec <- (runif(nrow(p)) <= p)
#    vec[vec] <- 1
#    if(is.factor(y)) {
#        vec <- factor(vec, c(0, 1), levels(y))
#    }
#    return(vec)
	stop("impute.logreg2 is disabled this release")
}

logitreg <- function(x, y, wt = rep(1, length(y)), intercept = TRUE, start, trace = TRUE, ...)
{
# logistic regression according to Venables & Ripley, p. 293

#    fmin <- function(x, y, w, beta) 
#    {
#        eta <- x %*% beta
#        p <- plogis(eta)
#        g <- -2 * dlogis(eta) * ifelse(y, 1/p, -1/(1-p))
#        value <- -2 * w * ifelse(y, logb(p), logb(1-p))
#        attr(value, "gradient") <- x * g * w
#        value   
#    }
#    if(is.null(dim(x))) dim(x) <- c(length(x), 1)
#    dn <- dimnames(x)[[2]]
#    if(!length(dn)) dn <- paste("Var", 1:ncol(x), sep="")
#    p <- ncol(x) + intercept
#    if (intercept) {
#        x <- cbind(1, x)
#        dn <- c("(Intercept)", dn)
#    }
#    if(missing(start)) start <- rep(0, p)
#
#    fit <- ms(~ fmin(x, y, wt, beta), start=list(beta=start), trace=trace, ...)
#    names(fit$par) <- dn
#    invisible(fit)
}



#-------------------------IMPUTE.POLYREG-----------------------------   
    
impute.polyreg<-function(y, ry, x)
{   
# impute.polyreg
#
# Imputation for categorical response variables by the Bayesian 
# polytomous regression model. See J.P.L. Brand (1999), Chapter 4,
# Appendix B.
#
# The method consists of the following steps:
# 1. Fit categorical response as a multinomial model 
# 2. Compute predicted categories
# 3. Add appropriate noise to predictions.
# 
# This algorithm uses the function multinom from the libraries nnet and MASS
# (Venables and Ripley). 
#
# Authors: Stef van Buuren, Karin Oudshoorn, 2000
#
#
    assign("data", cbind.data.frame(y, x), pos = 1)
    fit <- multinom(formula(data),data=data[ry,],maxit=200,trace=FALSE)
    post <- predict(fit, data[!ry,], type = "probs")
    remove("data", pos = 1)
    fy <- as.factor(y)
    nc <- length(levels(fy))
    un <- rep(runif(sum(!ry)),each=nc)
    idx <- 1+apply(un>apply(post,1,cumsum),2,sum)
    return(levels(fy)[idx])
}

    
#-------------------------IMPUTE.LDA-----------------------------   

impute.lda <- function(y, ry, x)
{
# impute.lda
#
# Imputation of categorical response variables by linear discriminant
# analysis. This function uses the Venables/Ripley functions
# lda and predict.lda to compute posterior probabilities for
# each incomplete case, and draws the imputations from this 
# posterior.
#
# Authors: Stef van Buuren, Karin Oudshoorn, 11 okt 1999
#
    fy <- as.factor(y)
    nc <- length(levels(fy))
    fit <- lda(x, fy, subset=ry)
    post <- predict(fit, x[!ry,,drop=FALSE])$posterior
    un <- rep(runif(sum(!ry)),each=nc)
    idx <- 1+apply(un>apply(post,1,cumsum),2,sum)
    return(levels(fy)[idx])
}
    
    
#--------------------------------------IMPUTE.MEAN------------------------------

impute.mean<-function(y, ry, x=NULL)
# impute.mean
#
# Unconditional mean imputation 
#
{
    return(rep(mean(y[ry]), times = sum(!ry)))
}

#--------------------------------------IMPUTE.SAMPLE------------------------------

impute.sample<-function(y, ry, x=NULL)
# impute.sample
#
# Generates random sample from the observed y's
#
{   
    return(sample(y[ry], size=sum(!ry), replace=TRUE))
}
#------------------------------IMPUTE.PASSIVE------------------------------------

impute.passive <- function(data, func)
#
# Special elementary imputation method for transformed data.
{
    return(model.frame(func, data))
}




#------------------------------COMPLETE------------------------------------

complete <- function(x, action = 1)
{
# complete
# Takes an object of class "mids", fills in the missing data, and
# return the completed data in the format specified by action.
# If action is a scalar between 1 and x$m, the function 
# returns the data with the action's imputation filled in.
# action can also be one of the following strings:
# "long"    produces a long matrix with nrow(x)*x$m
#       rows, containing all imputed data plus 
#       two additional variables ".id" (containing the 
#       row.names and ".imp" (containing the imputation 
#       number).
# "broad"   produces a broad matrix with ncol(x)*x$m
#       columns. The first ncol(x) columns contain the first
#       imputed data matrix. Column names are changed to reflect the
#       imputation number.
# "repeated"    produces a broad matrix with ncol(x)*x$m
#       columns. The first x$m columns give the filled-in
#       first variable. Column names are changed to reflect the 
#       imputation number.
#
#   Authors: S van Buuren, K Oudshoorn
#   Copyright (c) 1999 TNO Prevention and Health
#   Adapted for data frames 15 okt 99
#
    if(!is.mids(x)) stop("Input data must have class 'mids'.")
    if(is.numeric(action) && action >= 1 && action <= x$m) {
        data <- x$data
        mis <- is.na(data)
        ind <- (1:ncol(data))[colSums(mis) > 0]
        for(j in ind) {         
            data[mis[, j], j] <- x$imp[[j]][, action]
        }
        return(data)
    }
    code <- pmatch(action, c("long", "broad", "repeated"))
    if(!is.na(code) && code == 1) {
# long
        m <- x$m
        nr <- nrow(x$data)
        nc <- ncol(x$data)
        data <- as.data.frame(matrix(0, nrow = nr * m, ncol = nc + 2))
        for(j in nr * 1:m) {
            data[(j + 1 - nr):j, 1:nc] <- Recall(x, j/nr)   # recursive
        }
        for (j in 1:nc) {
            levels(data[,j]) <- levels(x$data[,j])
        }
        data[, nc + 1] <- rep(row.names(x$data), m)
        data[, nc + 2] <- rep(1:m, rep(nr, m))
        names(data) <- c(names(x$data), ".id", ".imp")
        return(data)
    }
    if(!is.na(code) && code == 2) {
# broad
        m <- x$m
        nr <- nrow(x$data)
        nc <- ncol(x$data)
        data <- as.data.frame(matrix(nrow = nr, ncol = nc * m))
        for(j in nc * 1:m)
            data[, (j + 1 - nc):j] <- Recall(x, j/nc)   # recursive
        names(data) <- paste(rep(names(x$data), m), rep(1:m, rep(nc, m)), sep = ".")
        return(data)
    }
    if(!is.na(code) && code == 3) {
# repeated
        data <- Recall(x, "broad")
        data <- data[, order(rep(1:ncol(x$data), x$m))]
        return(data)
    }
    stop("Argument action not recognized. \n")
}




# pool.ssc
#
# Poolfunctions for the MICE library V1.2

#------------------------------pool.mira-------------------------------

pool <- function(object, method="smallsample")
{
# General pooling function for multiple imputation parameters
# object: an object of class mira (Multiple Imputed Repeated Analysis)
# 
# Stef van Buuren, Karin Oudshoorn, July 1999.
#
#   Check the arguments
#
    call <- match.call()
    if(!is.mira(object)) stop("The object must have class 'mira'")
    if((m <- length(object$analyses)) < 2) stop("At least two imputations are needed for pooling.\n")
#
#   Set up arrays
#
    analyses <- object$analyses
    k <- length(coef(analyses[[1]]))
    names <- names(coef(analyses[[1]]))
    qhat <- matrix(NA, nrow=m, ncol=k, dimnames=list(1:m,names))
    u <- array(NA, dim=c(m,k,k), dimnames=list(1:m,names,names))
#
#   Fill arrays
#
    for (i in 1:m){
            fit <- analyses[[i]]
            qhat[i,] <- coef(fit)
#            u[i,,] <- Varcov(fit)
            u[i,,] <- vcov(fit)     # changed by J. Fox
    }
# 
#   Compute within, between and total variances
#
    qbar <- apply(qhat, 2, mean)                                # (3.1.2)
    ubar <- apply(u, c(2,3), mean)                              # (3.1.3)
    e <- qhat - matrix(qbar, nrow=m, ncol=k,byrow=TRUE)
    b <- (t(e) %*% e) / (m-1)                               # (3.1.4)
    t <- ubar + (1 + 1/m) * b                               # (3.1.5)
#
#   compute scalar inference quantities
#
    r <- (1 + 1/m) * diag(b/ubar)                               # (3.1.7)
    f <- (1 + 1/m) * diag(b/t)                                  # fraction of missing information
    df <- (m - 1) * (1 + 1/r)^2                             # (3.1.6)
    if (method=="smallsample") {  # Barnard-Rubin adjustment
        dfc <- fit$df.residual
        df <- dfc / ((1-(f/(m+1)))/(1-f) + dfc/df)
    }
    names(r) <- names(df) <- names(f) <- names
    fit <- list(call=call,call1=object$call,call2=object$call1,nmis=object$nmis,m=m,qhat=qhat,u=u,qbar=qbar,ubar=ubar,b=b,t=t,r=r,df=df,f=f)
    oldClass(fit) <- if(.SV4.)'mipo' else c("mipo",oldClass(object)) ## FEH
    return(fit)
}

# auxilary.ssc
#
# Extra functions for the MICE library V1.2

#------------------------------MD.PATTERN-------------------------------

md.pattern <- function(x)
{
# md.pattern
#
# computes the missing data pattern in the data
# x can be a vector, matrix or data frame
# NA's indicate missing data
# based on Schafer's prelim.norm function
# SvB, 13-7-99
#
    if(!(is.matrix(x) | is.data.frame(x)))
        stop("Data should be a matrix or dataframe")
    if(ncol(x) < 2) stop("Data should have at least two columns")
    if(is.data.frame(x)) x <- data.frame.to.matrix(x)
    n <- nrow(x)
    p <- ncol(x)
    storage.mode(x) <- "single" # find missingness patterns
    r <- 1 * is.na(x)
    nmis <- as.integer(apply(r, 2, sum))
    names(nmis) <- dimnames(x)[[2]] # index the missing data patterns
    mdp <- as.integer((r %*% (2^((1:ncol(x)) - 1))) + 1)    # do row sort
    ro <- order(mdp)
    x <- matrix(x[ro,  ], n, p) ##pm 04/02
    mdp <- mdp[ro]
    r <- matrix(r[ro,  ], n, p) ##pm 04/02
    ro <- order(ro) # compress missing data patterns
    mdpst <- as.integer(seq(along = mdp)[!duplicated(mdp)])
    mdp <- unique(mdp)
    npatt <- length(mdpst)  # create r-matrix for display purposes
    r <- 1 - r
    r <- matrix(r[mdpst,  ], npatt, p)
    if(npatt == 1)
        tmp <- format(n)
    if(npatt > 1)
        tmp <- format(c(mdpst[2:npatt], n + 1) - mdpst)
    dimnames(r) <- list(tmp, dimnames(x)[[2]])
    storage.mode(r) <- "integer"    # center and scale the columns of x
#
    if(npatt > 1)
        nmdp <- as.integer(c(mdpst[-1], n + 1) - mdpst)
    if(npatt == 1) nmdp <- n    # 
# sort the rows and columns according to the marginals
    co <- order(nmis)
    ro2 <- order(nmis.row <- p - as.integer(apply(r, 1, sum)))
    r <- rbind(r[ro2, co], nmis[co])
    r <- cbind(r, c(nmis.row[ro2], sum(nmis)))
    r
}

#------------------------------DATA.FRAME.TO.MATRIX-------------------------------

#
data.frame.to.matrix <- 
function(x)
{
# adapted version of as.matrix.data.frame
# converts factor values to codes instead of labels
# return numeric matrix
# SvB, 12-7-99
    X <- x
    DD <- dim(X)
    dn <- dimnames(X)
    collabs <- as.list(dn[[2]])
    oldClass(X) <- NULL
    p <- DD[2]
    n <- DD[1]
    non.numeric <- non.atomic <- FALSE
    for(j in 1:p) {
        xj <- X[[j]]
        if(length(dj <- dim(xj)) == 2 && dj[2] > 1) {
            if(inherits(xj, "data.frame"))
                xj <- X[[j]] <- as.matrix(X[[j]])
            dnj <- dimnames(xj)[[2]]
            collabs[[j]] <- paste(collabs[[j]], if(length(dnj)) dnj else seq(1:dj[2]), sep = ".")
        }
        if(length(levels(xj)) > 0 || !(is.numeric(xj) || is.complex(xj)) || inherits(xj, "dates"))
            non.numeric <- TRUE
        if(!is.atomic(xj))
            non.atomic <- TRUE
    }
    if(non.atomic) {
        for(j in 1:p) {
            xj <- X[[j]]
            if(is.recursive(xj)) {
            }
            else X[[j]] <- as.list(as.vector(xj))
        }
    }
    else if(non.numeric) {
        for(j in 1:p) {
            xj <- X[[j]]
            if(length(levels(xj))) {
                X[[j]] <- as.vector(codes(xj))
            }
            else X[[j]] <- format(xj)
        }
    }
    if(length(unique(sapply(X, function(Xi)
    if(is.matrix(Xi)) nrow(Xi) else length(Xi)))) != 1)
        stop("Illegal data frame: lengths of columns differ")
    X <- unlist(X, recursive = FALSE, use.names = FALSE)
    X <- as.numeric(X)
    dim(X) <- c(n, length(X)/n)
    dimnames(X) <- list(dn[[1]], unlist(collabs, use.names = FALSE))
    oldClass(X) <- "matrix"
    X
}


#--------------------------------IS.MIDS--------------------------------------

is.mids<-function(data)
{
    inherits(data,"mids")
}


#--------------------------------IS.MIRA--------------------------------------

is.mira <- 
function(data)
{
    inherits(data, "mira")
}

#--------------------------------IS.MIPO--------------------------------------

is.mipo <- 
function(data)
{
    inherits(data, "mipo")
}

#------------------------------is.passive------------------------------------

is.passive <- function(string)
{
    return("~" == substring(string, 1, 1))
}



#--------------------------------PRINT.MIDS--------------------------------------

print.mids<-function(x, ...)
{
    if (is.mids(x)) {
        cat("Multiply imputed data set")
        cat("\nCall:\n")
        print(x$call)
        cat("Number of multiple imputations: ",x$m)
        cat("\nMissing cells per column:\n")
        print(x$nmis)
        cat("Imputation methods:\n")
        print(x$imputationMethod)
        cat("VisitSequence:\n")
        print(x$visitSequence)
        cat("PredictorMatrix:\n")
        print(x$predictorMatrix)
        cat("Random generator seed value: ",x$seed,"\n")
    }
    else print(x)
    invisible()
}
#------------------------------print.mira-------------------------------

print.mira <- function(x,... )
{
 ## prints the mira object; A mira object
 ## is in fact a list, so it will be printed as such.
 ## KO, 3/2/00

    if(is.mira(x))
        print.listof(x) ##PM 4/02
    else  print(x) 
    invisible()

}

#------------------------------summary.mira-------------------------------
summary.mira<-function(object, correlation = TRUE, ...)
{
# This summary function is for a mira object. 
# Then the seperate analyses are of class lm (glm), it calls
# sequentially summary.lm (summary.glm) for all analyses. 
# KO, 4/2/00

    for (i in (1:length(object$analyses))){ 
        cat("\n","##summary analyses of imputation",i,":\n")
        print(summary(object$analyses[[i]]))
    }
}

#------------------------------print.mipo-------------------------------
print.mipo <- function(x, ...)
{
    if(!is.null(x$call)) {
        cat("Call: ")
        dput(x$call)
    }
    cat("\nPooled coefficients:\n")
    print(x$qbar, ...)
#   cat("Relative increase in variance due to nonresponse per parameter:", "\n")
#   print(x$r)
    cat("\nFraction of information about the coefficients missing due to nonresponse:", "\n")
    print(x$f)
    invisible(x)
}


#------------------------------summary.mipo-------------------------------

summary.mipo <- function(object, ...){
# 
# summary method for the pooled analysis results
#
# object: object of class mipo
#
    table <- array(object$qbar, dim = c(length(object$qbar), 9))
    dimnames(table) <- list(labels(object$qbar), c("est", "se", "t", "df", "Pr(>|t|)", "lo 95","hi 95", "missing", "fmi"))
    table[, 2] <- sqrt(diag(object$t))
    table[, 3] <- table[,1] / table[,2]
    table[, 4] <- object$df
    table[, 5] <- if(all(object$df) > 0) 2 * (1 - pt(abs(table[, 3]), object$df)) else NA
    table[, 6] <- table[,1] - qt(0.975, object$df) * table[, 2]
    table[, 7] <- table[,1] + qt(0.975, object$df) * table[, 2]
    table[, 8] <- object$nmis[names(object$qbar)]
    table[, 9] <- object$f
    return(table)
}


#--------------------------------SUMMARY.MIDS--------------------------------------

summary.mids<-function(object, ...)
{
    print(object)
    invisible()
}


#-------------------------------PLOT.MIDS---------------------------------

plot.mids <-function(x, y, ...){
    names <- dimnames(x$chainVar[,,1])[[1]]
    s.mat <- sqrt(x$chainVar)
    m.mat <- x$chainMean
    
    par(mfrow=c((dim(m.mat)[1]),2))
    ylimits1.min<-apply(m.mat,1,min)
    ylimits1.max<-apply(m.mat,1,max)
    ylimits2.min<-apply(s.mat,1,min)
    ylimits2.max<-apply(s.mat,1,max)
    for (m in 1:(dim(m.mat)[1])){
        plot(1:(dim(m.mat)[2]),m.mat[m,,1],type="n",ylim=c(ylimits1.min[m],ylimits1.max[m]),xlab="iteration",ylab="mean")
title(names[m])
for (i in 1:(dim(m.mat)[3])) lines(1:(dim(m.mat)[2]),m.mat[m,,i],col=i)
        plot(1:(dim(s.mat)[2]),s.mat[m,,1],type="n",ylim=c(ylimits2.min[m],ylimits2.max[m]),xlab="iteration",ylab="sd")
title(names[m])
        for (i in 1:(dim(s.mat)[3])) lines(1:(dim(s.mat)[2]),s.mat[m,,i],col=i)
    }

}



#-------------------------------LM.MIDS---------------------------------

"lm.mids" <- function(formula, data, weights, subset, na.action, method = "qr", model = FALSE, x
     = FALSE, y = FALSE, contrasts = NULL, ...) 
{
#  adapted 28/1/00
#  repeated complete data regression (lm) on a mids data set
#
    call <- match.call()
    if(!is.mids(data))
        stop("The data must have class mids")
    analyses <- as.list(1:data$m)   #
#  do the repated analysis, store the result
#
    for(i in 1:data$m) {
        data.i <- complete(data, i)
        analyses[[i]] <- lm(formula, data = data.i, ...)
    }
#
# return the complete data analyses as a list of length nimp
#
    object <- list(call = call, call1 = data$call, nmis = data$nmis, analyses = analyses)
    oldClass(object) <- if(.SV4.)'mira' else c("mira", "lm")  ## FEH
    return(object)
}

#-------------------------------GLM.MIDS---------------------------------

"glm.mids" <- function(formula = formula(data), family = gaussian, data = sys.parent(), weights, 
    subset, na.action, start = eta, control = glm.control(...), method = "glm.fit", 
    model = FALSE, x = FALSE, y = TRUE, contrasts = NULL, ...)
{
#  adapted 04/02/00
#  repeated complete data regression (glm) on a mids data set
#
    call <- match.call()
    if(!is.mids(data))
        stop("The data must have class mids")
    analyses <- as.list(1:data$m)   #
#  do the repated analysis, store the result
#
    for(i in 1:data$m) {
        data.i <- complete(data, i)
        analyses[[i]] <- glm(formula, data = data.i, ...)
    }
#
# return the complete data analyses as a list of length nimp
#
    object <- list(call = call, call1 = data$call, nmis = data$nmis, analyses = analyses)
    oldClass(object) <- if(.SV4.) 'mira' else c("mira", "glm","lm") ## FEH
    return(object)
}


#
# System functions for the MICE library V1.14

#------------------------------.First.lib-------------------------------
.First.lib <- function(library,section){
    cat("MICE V1.14 library            Copyright (2004) TNO Prevention and Health, Leiden\n")
    cat("This library is distributed under the GNU General Public License (version 2)\n")
    if(.R.) {
      require(Hmisc)      ##PM 04/02
      require(MASS)
      require(nnet)
      require(nlme)				#RJ
    } else {
    library(Hmisc)
    library(MASS)
    library(nnet)
    library(nlme)				#RJ
    }
    invisible()
}







# This file contains the R code used in 
# 
# Van Buuren, S., Groothuis-Oudshoorn, K. (2009) 
# MICE: Multivariate Imputation by Chained Equations in R. 
# Journal of Statistical Software, forthcoming.
# date: 27/08/2009
# See: http://www.stefvanbuuren.nl/publications/MICE in R - Draft.pdf
#
# adapted for V2.4 17/10/2010
# The code assumes mice V2.3 or higher.
if (as.numeric(packageDescription("mice")$Version)<2.3) warning("MICE V2.3 needed")

# 2.4 Simple example
#
# inspect missing data
library(mice)
nhanes
md.pattern(nhanes)
p <- md.pairs(nhanes)
p

# two missing data plots
library(VIM)
marginplot(nhanes[,c("chl","bmi")], col=c("blue","red","orange"), cex=1.5, cex.lab=1.5, cex.numbers=1.3, pch=19)
pbox(nhanes,pos=1,int=FALSE,cex=1.2)

# impute
imp <- mice(nhanes)
imp
imp$imp$bmi
complete(imp)

# plot data and imputations
library(lattice)
com <- complete(imp, "long", inc=T)
col <- rep(c("blue","red")[1+as.numeric(is.na(imp$data$chl))],6)
stripplot(chl~.imp, data=com, jit=TRUE, fac=0.8, col=col, pch=20, cex=1.4, xlab="Imputation number")

# do the analysis of interest
fit <- with(imp, lm(chl~age+bmi))

# pool the results
pool(fit)
summary(pool(fit))


#  3.2 Elementary imputation methods
#  
# 
imp <- mice(nhanes, method = "norm")
imp <- mice(nhanes, meth = c("", "norm", "pmm", "mean"))
str(nhanes2)
imp <- mice(nhanes2, me=c("polyreg","pmm","logreg","norm"))
# next statement errors on purpose
# imp <- mice(nhanes2, meth=c("","","logreg","norm"))
mice(nhanes, defaultMethod = c("norm","logreg","polyreg"))


#  3.3 Predictor selection
#
#
imp <- mice(nhanes, print=FALSE)
imp$predictorMatrix

# removing a predictor
pred <- imp$predictorMatrix
pred[,"bmi"] <- 0
pred
imp <- mice(nhanes, pred=pred, pri=F)

# skipping imputation
ini <- mice(nhanes2, maxit=0, pri=F)
pred <- ini$pred
pred[,"bmi"] <- 0
meth <- ini$meth
meth["bmi"] <- ""
imp <- mice(nhanes2, meth=meth, pred=pred, pri=F)
imp$imp$bmi

# intercept imputation
ini <- mice(nhanes2, maxit=0, pri=F)
pred <- ini$pred
pred["bmi",] <- 0
imp <- mice(nhanes2, pred=pred, pri=F)
imp$imp$bmi

# multilevel imputation
# ------------- linear multilevel imputation
# -- dependent: pupil popularity
# -- fixed: teacher experience
# -- random: intercept, sex
# -- class: school
popmis[1:3,]
popmis2 <- popmis[,-6]
ini <- mice(popmis2, maxit=0)
pred <- ini$pred
pred["popular",] <- c(0, -2, 0, 2, 1, 0)
imp <- mice(popmis2, meth=c("","","2l.norm","","",""), pred=pred, maxit=1)

# quick predictor selection
#
#
cor(nhanes, use="pairwise.complete.obs")
cor(y=nhanes, x=!is.na(nhanes), use="pairwise.complete.obs")
p <- md.pairs(nhanes)
p$mr/(p$mr+p$mm)
quickpred(nhanes)
imp <- mice(nhanes, pred=quickpred(nhanes, minpuc=0.25, include="age"))


# 3.4 Passive imputation
#
#
# preserving a transformation
nhanes2.ext <- cbind(nhanes2, lchl=log(nhanes2$chl))
ini <- mice(nhanes2.ext,max=0,pri=FALSE)
meth <- ini$meth
meth["lchl"] <- "~log(chl)"
pred <- ini$pred
pred[c("hyp","chl"),"lchl"] <- 0
pred["bmi","chl"] <- 0
pred
imp <- mice(nhanes2.ext, meth=meth, pred=pred)
complete(imp)

# index of two variables
nhanes2.ext <- cbind(nhanes2, lchl=NA)
md.pattern(boys[,c("hgt","wgt","bmi")])
ini <- mice(boys,max=0,print=FALSE)
meth <- ini$meth
meth["bmi"] <- "~I(wgt/(hgt/100)^2)"
pred <- ini$pred
pred[c("wgt","hgt","hc","reg"),"bmi"] <- 0
pred[c("gen","phb","tv"),c("hgt","wgt","hc")] <- 0
pred
imp <- mice(boys, pred=pred, meth=meth, maxit=20)
complete(imp)[is.na(boys$bmi),]
meth["bmi"] <- "~round(wgt/(hgt/100)^2,dig=2)"

# sum scores
ini <- mice(cbind(boys,mat=NA),max=0,print=FALSE)
meth <- ini$meth
meth["mat"] <- "~I(as.integer(gen)+as.integer(phb)+as.integer(cut(tv,breaks=c(0,3,6,10,15,20,25))))"
meth["bmi"] <- "~I(wgt/(hgt/100)^2)"
pred <- ini$pred
pred[c("gen","phb","tv"),"mat"] <- 0
pred[c("hgt","wgt","hc","reg"),c("gen","phb","tv")] <- 0
pred[c("wgt","hgt","hc","reg"),"bmi"] <- 0
pred[c("gen","phb","tv"),c("hgt","wgt","hc")] <- 0
pred[c("bmi","mat"),] <- 0
pred
imp <- mice(cbind(boys,mat=NA), pred=pred, meth=meth, maxit=20)
col <- c("blue","red")[1+as.numeric(is.na(boys$gen)|is.na(boys$phb)|is.na(boys$tv))] 
with(complete(imp,1),plot(age,mat,type="n",ylab="Maturation sum score",xlab="Age (years)"))
with(complete(imp,1),points(age,mat,col=col))

# interactions continuous variables
nhanes2.ext <- cbind(nhanes2, bmi.chl=NA)
ini <- mice(nhanes2.ext, max=0, print=FALSE)
meth <- ini$meth
meth["bmi.chl"] <- "~I((bmi-25)*(chl-200))"
pred <- ini$pred
pred[c("bmi","chl"),"bmi.chl"] <- 0
imp <- mice(nhanes2.ext, meth=meth, pred=pred)
imp$pad$data

# interactions categorical variables
nhanes2.ext <- cbind(nhanes2,age.1.bmi=NA,age.2.bmi=NA)
ini <- mice(nhanes2.ext, max=0, print=FALSE)
meth <- ini$meth
meth["age.1.bmi"] <- "~I(age.1*(bmi-25))"
meth["age.2.bmi"] <- "~I(age.2*(bmi-25))"
pred <- ini$pred
pred[c("age","bmi"),c("age.1.bmi","age.2.bmi")] <- 0
imp <- mice(nhanes2.ext, meth=meth, pred=pred, maxit=10)

# squeeze
nhanes2.ext <- cbind(nhanes2, lchl=NA)
ini <- mice(nhanes2.ext,max=0,pri=FALSE)
meth <- ini$meth
meth[c("lchl","chl")] <- c("~log(chl)","norm")
pred <- ini$pred
pred[c("hyp","chl"),"lchl"] <- 0
pred["bmi","chl"] <- 0
# next statement produces an error in iteration 6
# imp <- mice(nhanes2.ext, meth=meth, pred=pred, seed=1, maxit=10)
meth["lchl"] <- "~log(squeeze(chl,bounds=c(100,300)))"
imp <- mice(nhanes2.ext, meth=meth, pred=pred, seed=1, maxit=10)

# post-processing: prevent negative chl
nhanes2.ext <- cbind(nhanes2, lchl=NA)
ini <- mice(nhanes2.ext,max=0,pri=FALSE)
meth <- ini$meth
meth[c("lchl","chl")] <- c("~log(chl)","norm")
pred <- ini$pred
pred[c("hyp","chl"),"lchl"] <- 0
pred["bmi","chl"] <- 0
post <- ini$post
post["chl"] <- "imp[[j]][,i] <- squeeze(imp[[j]][,i],c(100,300))"
imp <- mice(nhanes2.ext, meth=meth, pred=pred, post=post, seed=1, maxit=10)
imp$imp$chl

# post-processing: constrain maturation scores below age 5 yrs
ini <- mice(cbind(boys,mat=NA),max=0,print=FALSE)
meth <- ini$meth
meth["mat"] <- "~I(as.integer(gen)+as.integer(phb)+as.integer(cut(tv,breaks=c(0,3,6,10,15,20,25))))"
meth["bmi"] <- "~I(wgt/(hgt/100)^2)"
pred <- ini$pred
pred[c("gen","phb","tv"),"mat"] <- 0
pred[c("hgt","wgt","hc","reg"),c("gen","phb","tv")] <- 0
pred[c("wgt","hgt","hc","reg"),"bmi"] <- 0
pred[c("gen","phb","tv"),c("hgt","wgt","hc")] <- 0
pred[c("bmi","mat"),] <- 0
post <- ini$post
post["gen"] <- "imp[[j]][p$data$age[!r[,j]]<5,i] <- levels(boys$gen)[1]"
post["phb"] <- "imp[[j]][p$data$age[!r[,j]]<5,i] <- levels(boys$phb)[1]"
post["tv"]  <- "imp[[j]][p$data$age[!r[,j]]<5,i] <- 1"
imp <- mice(cbind(boys,mat=NA), pred=pred, meth=meth, post=post, maxit=10)

col <- c("blue","red")[1+as.numeric(is.na(boys$gen)|is.na(boys$phb)|is.na(boys$tv))] 
with(complete(imp,1),plot(age,mat,type="n",ylab="Maturation sum score",xlab="Age (years)"))
with(complete(imp,1),points(age,mat,col=col))
 
  
# 3.6 Visiting scheme
#
#
nhanes2.ext <- cbind(nhanes2, bmi.chl=NA)
ini <- mice(nhanes2.ext, max=0, print=FALSE)
meth <- ini$meth
meth["bmi.chl"] <- "~I((bmi-25)*(chl-200))"
pred <- ini$pred
pred[c("bmi","chl"),"bmi.chl"] <- 0
imp <- mice(nhanes2.ext, meth=meth, pred=pred)

vis<-imp$vis
vis<-append(vis,vis[4],1)
vis
imp <- mice(nhanes2.ext, meth=meth, pred=pred, vis=vis)
imp <- mice(nhanes2.ext, meth=meth, pred=pred, vis=c(2,4,5,3))
imp <- mice(nhanes2.ext, meth=meth, pred=pred, vis="monotone")
  
  
# 4.1 Dry run
#
#
ini <- mice(nhanes2, maxit=0)

import <- matrix(c(30,30,30,29,25,21,25,25,22,33,27,22,27,35,27,20,27,30),byrow=T,nr=9)
imp <- mice(nhanes)
imp$imp$bmi[,1:2] <- import
imp$imp$bmi


# 4.2 Step by Step
#
#
imp1 <- mice(nhanes, maxit=1, seed=612)
a <- runif(10)
imp2 <- mice.mids(imp1, maxit=3)
imp <- mice(nhanes, maxit=4, seed=612)
all(imp2$imp$bmi==imp$imp$bmi)   # should be TRUE


# 4.3 Assessing convergence
# exact triplet - pathological case
ini <- mice(boys,max=0,print=FALSE)
meth <- ini$meth
meth["bmi"] <- "~I(wgt/(hgt/100)^2)"
meth["wgt"] <- "~I(bmi*(hgt/100)^2)"
meth["hgt"] <- "~I(100*sqrt(wgt/bmi))"
imp <- mice(boys, meth=meth, maxit=20)
plot(imp)

# second pathological example
ini <- mice(boys,max=0,print=FALSE)
meth <- ini$meth
meth["bmi"] <- "~I(wgt/(hgt/100)^2)"
imp <- mice(boys, meth=meth, maxit=20)
plot(imp)

# rerun from section 3.3 (sum scores) - healthy convergence
ini <- mice(cbind(boys,mat=NA),max=0,print=FALSE)
meth <- ini$meth
meth["mat"] <- "~I(as.integer(gen)+as.integer(phb)+as.integer(cut(tv,breaks=c(0,3,6,10,15,20,25))))"
meth["bmi"] <- "~I(wgt/(hgt/100)^2)"
pred <- ini$pred
pred[c("gen","phb","tv"),"mat"] <- 0
pred[c("hgt","wgt","hc","reg"),c("gen","phb","tv")] <- 0
pred[c("wgt","hgt","hc","reg"),"bmi"] <- 0
pred[c("gen","phb","tv"),c("hgt","wgt","hc")] <- 0
pred[c("bmi","mat"),] <- 0
pred
imp <- mice(cbind(boys,mat=NA), pred=pred, meth=meth, maxit=20)
plot(imp)

# monitor Kendall's tau between gen and phb

### Oct 2010: We need to define a conversion function
### since cor() does not allow factors anymore (even not
### for spearman or kendall)
fac2int <- function(f){
  if (!is.data.frame(f)) stop("Sorry: Only dataframes")
  z <- f
  for (j in 1:length(f))
    { if (is.factor(f[[j]])) z[[j]] <- as.integer(f[[j]])
    }
  return(z)
}
  
imp <- mice(cbind(boys,mat=NA), meth=meth, pred=pred, maxit=0, seed=5222)
tau <- matrix(NA,nr=20,nc=5)
for (i in 1:20) {
    imp <- mice.mids(imp, maxit=1)
    x <- complete(imp,"repeated")[,paste("gen",1:5,sep=".")]
    y <- complete(imp,"repeated")[,paste("phb",1:5,sep=".")]
    x <- fac2int(x)   # added Oct 2010
    y <- fac2int(y)   # added Oct 2010
    tau[i,] <- diag(cor(x=x,y=y,method="kendall"))
}
matplot(x=1:20,y=tau,xlab="Iteration",type="l")

# 4.5 Checking your imputations
# 
# 
# same as before (section assessing convergence)    
ini <- mice(cbind(boys,mat=NA,hc.na=is.na(boys$hc)),max=0,print=FALSE)
meth <- ini$meth
meth["mat"] <- "~I(as.integer(gen)+as.integer(phb)+as.integer(cut(tv,breaks=c(0,3,6,10,15,20,25))))"
meth["bmi"] <- "~I(wgt/(hgt/100)^2)"
pred <- ini$pred
pred[c("gen","phb","tv"),"mat"] <- 0
pred[c("hgt","wgt","hc","reg"),c("gen","phb","tv")] <- 0
pred[c("wgt","hgt","hc","reg"),"bmi"] <- 0
pred[c("gen","phb","tv"),c("hgt","wgt","hc")] <- 0
pred[c("bmi","mat"),] <- 0
pred[,"hc.na"] <- 0
pred
imp <- mice(cbind(boys,mat=NA,hc.na=is.na(boys$hc)), pred=pred, meth=meth, maxit=20)

library(lattice)
long <- complete(imp,"long")
levels(long$.imp) <- paste("Imputation",1:5)
long <- cbind(long, hc.na=is.na(imp$data$hc))
densityplot(~hc|.imp, data=long, group=hc.na, plot.points=FALSE,
		ref=TRUE, xlab="Head circumference",scales=list(y=list(draw=F)),
    par.settings=simpleTheme(col.line=rep(c("blue","red"))),
    auto.key = list(columns=2,text=c("Observed","Imputed")))

# conditional propensity score plot
fit.hc <- with(imp, glm(hc.na~age+wgt+hgt+mat+reg,family=binomial))
ps <- rowMeans(sapply(fit.hc$analyses, fitted.values))
hc.1 <- complete(imp,1)$hc
hc.na <- is.na(imp$data$hc)
xyplot(hc.1 ~ ps, groups=hc.na, 	
   xlab="Probability of missing head circumference",
   ylab="Head circumference",
   par.settings = simpleTheme(col.points = rep(c("blue","red")), 
   cex=1.2, pch=19),
   auto.key = list(columns=2, text = c("Observed","Imputed")))

## Kernel density estimates of the distributions of the residuals
## from the regression of the observe/imputed values on the
## propensity scores
fit <- lm(hc.1 ~ poly(ps,4))
summary(fit)
densityplot(~ residuals(fit), group = hc.na, plot.points = FALSE,
	ref = TRUE, xlab = "Residuals of regression of hc on propensity score",
      scales = list(y=list(draw=F)),
      par.settings = simpleTheme(col.line = rep(c("blue","red"))),
      auto.key = list(columns = 2, text=c("Observed","Imputed")))


# 5.1 Repeated data analysis
#
#
imp <- mice(nhanes2)
fit <- with(imp, lm(chl~age+bmi))
summary(fit)
fit$ana[[3]]

expr <- expression(ov<-cut(bmi,c(10,25,50)),table(age,ov))
fit <- with(imp, eval(expr))   
fit$an
    
com <- complete(imp, 3)
com <- complete(imp, "long")
com <- complete(imp, "long", include=TRUE)
by(cbind(age=com$age,ov=cut(com$bmi,c(10,25,50))), com$.imp, table)

# 5.3 Pooling
#
#
imp <- mice(nhanes2)
fit <- with(imp, lm(chl~age+bmi))
est <- pool(fit)
est

methods(coef)
methods(vcov)

# Model testing - Wald method
imp <- mice(nhanes2, pri=F)
fit0 <- with(data=imp,expr=lm(bmi~age+hyp))
fit1 <- with(data=imp,expr=lm(bmi~age+hyp+chl))
stat <- pool.compare(fit1, fit0, method="Wald")
stat$p

# Model testing - LLR method
imp <- mice(boys, pri=F)
fit0 <- with(data=imp, expr=glm(I(gen>levels(gen)[1])~hgt+hc,family=binomial))
fit1 <- with(data=imp, expr=glm(I(gen>levels(gen)[1])~hgt+hc+reg,family=binomial))
stat <- pool.compare(fit1, fit0, method="likelihood", data=imp)
stat$p


# 6.1 Adding your own imputation functions
#
#

# first define mice.impute.myfunc
# mice(nhanes, method="myfunc")


# 6.2 Sensitivity analysis under MNAR
#
#
ini <- mice(nhanes2,max=0,pri=FALSE)
post <- ini$post
k <- seq(1,1.5,0.1)
est <- vector("list",length(k))
for (i in 1:length(k)) {
  post["chl"] <- paste("imp[[j]][,i] <-",k[i],"* imp[[j]][,i]")
  imp <- mice(nhanes2, post=post, seed=10, maxit=20)
  fit <- with(imp, lm(bmi~age+chl))
  est[[i]] <- summary(pool(fit))
}
  
  
# 7.2 SPSS
# 
# The code below should not be run from R, but from SPSS.
# Remove one (1) # sign per line, and run.
# 
# BEGIN PROGRAM R.
# Instruct R to import the data and the dictionary from SPSS.
#dict <- spssdictionary.GetDictionaryFromSPSS()
#data <- spssdata.GetDataFromSPSS()
## Load mice, impute and get the original + imputed data.
#library(mice)
#imp <- mice(data,maxit=10)
#com <- complete(imp,"long",inc=TRUE)
#com <- cbind(com, Imputation_ = as.integer(com$.imp)-1)
## Export imputed data and dictionary from R to SPSS.
#spssdictionary.SetDictionaryToSPSS("com", dict)
#spssdata.SetDataToSPSS("com", com)
#spssdictionary.EndDataStep()
#END PROGRAM.
  
# 7.3 MItools
#
# 
library(mitools)
mydata <- imputationList(lapply(1:5, complete, x=imp))
# fit <- with(mydata, expr=lm(..~.. + ..)))


# 7.4 Zelig
#
#
library(Zelig)
imp <- mice(boys, maxit=2)
imp <- cbind.mids(imp,data.frame(Rhc=is.na(boys$hc)))
mydata2 <- mi(complete(imp,1), complete(imp,2), complete(imp,3), complete(imp,4),complete(imp,5))
fit <- zelig(Rhc~age+wgt+hgt+bmi+gen+phb+tv+reg, model="logit", data=mydata2)
summary(fit)





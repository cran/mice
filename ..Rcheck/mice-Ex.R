### * <HEADER>
###
attach(NULL, name = "CheckExEnv")
assign("nameEx", 
       local({
	   s <- "__{must remake R-ex/*.R}__"
           function(new) {
               if(!missing(new)) s <<- new else s
           }
       }),
       pos = "CheckExEnv")
## Add some hooks to label plot pages for base and grid graphics
assign("base_plot_hook",
       function() {
           pp <- par(c("mfg","mfcol","oma","mar"))
           if(all(pp$mfg[1:2] == c(1, pp$mfcol[2]))) {
               outer <- (oma4 <- pp$oma[4]) > 0; mar4 <- pp$mar[4]
               mtext(sprintf("help(\"%s\")", nameEx()), side = 4,
                     line = if(outer)max(1, oma4 - 1) else min(1, mar4 - 1),
              outer = outer, adj = 1, cex = .8, col = "orchid", las=3)
           }
       },
       pos = "CheckExEnv")
assign("grid_plot_hook",
       function() {
           pushViewport(viewport(width=unit(1, "npc") - unit(1, "lines"),
                                 x=0, just="left"))
           grid.text(sprintf("help(\"%s\")", nameEx()),
                     x=unit(1, "npc") + unit(0.5, "lines"),
                     y=unit(0.8, "npc"), rot=90,
                     gp=gpar(col="orchid"))
       },
       pos = "CheckExEnv")
setHook("plot.new",     get("base_plot_hook", pos = "CheckExEnv"))
setHook("persp",        get("base_plot_hook", pos = "CheckExEnv"))
setHook("grid.newpage", get("grid_plot_hook", pos = "CheckExEnv"))
assign("cleanEx",
       function(env = .GlobalEnv) {
	   rm(list = ls(envir = env, all.names = TRUE), envir = env)
           RNGkind("default", "default")
	   set.seed(1)
   	   options(warn = 1)
	   .CheckExEnv <- as.environment("CheckExEnv")
	   delayedAssign("T", stop("T used instead of TRUE"),
		  assign.env = .CheckExEnv)
	   delayedAssign("F", stop("F used instead of FALSE"),
		  assign.env = .CheckExEnv)
	   sch <- search()
	   newitems <- sch[! sch %in% .oldSearch]
	   for(item in rev(newitems))
               eval(substitute(detach(item), list(item=item)))
	   missitems <- .oldSearch[! .oldSearch %in% sch]
	   if(length(missitems))
	       warning("items ", paste(missitems, collapse=", "),
		       " have been removed from the search path")
       },
       pos = "CheckExEnv")
assign("ptime", proc.time(), pos = "CheckExEnv")
grDevices::postscript("mice-Ex.ps")
assign("par.postscript", graphics::par(no.readonly = TRUE), pos = "CheckExEnv")
options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"))
options(warn = 1)    
library('mice')

assign(".oldSearch", search(), pos = 'CheckExEnv')
assign(".oldNS", loadedNamespaces(), pos = 'CheckExEnv')
cleanEx(); nameEx("complete");
### * complete

flush(stderr()); flush(stdout())

### Name: complete
### Title: Produces Imputed Flat Files from Multiply Imputed Data Set
###   (mids)
### Aliases: complete
### Keywords: misc

### ** Examples

data(nhanes)
imp <- mice(nhanes)     # do default multiple imputation on a numeric matrix
mat <- complete(imp)    # fills in the first imputation
mat <- complete(imp, 3) # fills in the third imputation
mat <- complete(imp, "long") # produces a long matrix with stacked complete data
mat <- complete(imp, "b") # a broad matrix
cor(mat)                # for numeric mat, produces a blocked correlation matrix, where
            # each m*m block contains of the same variable pair over different
            # multiple imputations.



cleanEx(); nameEx("glm.mids");
### * glm.mids

flush(stderr()); flush(stdout())

### Name: glm.mids
### Title: Generelized Linear Regression on Multiply Imputed Data
### Aliases: glm.mids
### Keywords: misc

### ** Examples

data(nhanes)
imp <- mice(nhanes)     # do default multiple imputation on a numeric matrix
glm.mids((hyp==2)~bmi+chl,data=imp)
    # fit
    # $call:
    # glm.mids(formula = (hyp == 2) ~ bmi + chl, data = imp)
    # 
    # $call1:
    # mice(data = nhanes)
    # 
    # $nmis:
    #  age bmi hyp chl 
    #    0   9   8  10
    # 
    # $analyses:
    # $analyses[[1]]:
    # Call:
    # glm(formula = formula, data = data.i)
    # 
    # Coefficients:
    #  (Intercept)         bmi         chl 
    #   -0.4746337 -0.01565534 0.005417846
    # 
    # Degrees of Freedom: 25 Total; 22 Residual
    # Residual Deviance: 2.323886 
    # 
    # $analyses[[2]]:
    # Call:
    # glm(formula = formula, data = data.i)
    # 
    # Coefficients:
    #  (Intercept)         bmi         chl 
    #   -0.1184695 -0.02885779 0.006090282
    # 
    # Degrees of Freedom: 25 Total; 22 Residual
    # Residual Deviance: 3.647927 
    # 
    # $analyses[[3]]:
    # Call:
    # glm(formula = formula, data = data.i)
    # 
    # Coefficients:
    #  (Intercept)          bmi         chl 
    #   -0.1503616 -0.003002851 0.002130091
    # 
    # Degrees of Freedom: 25 Total; 22 Residual
    # Residual Deviance: 3.799126 
    # 
    # $analyses[[4]]:
    # Call:
    # glm(formula = formula, data = data.i)
    # 
    # Coefficients:
    #  (Intercept)        bmi         chl 
    #  0.009442083 -0.0237619 0.004631881
    # 
    # Degrees of Freedom: 25 Total; 22 Residual
    # Residual Deviance: 3.874522 
    # 
    # $analyses[[5]]:
    # Call:
    # glm(formula = formula, data = data.i)
    # 
    # Coefficients:
    #  (Intercept)         bmi         chl 
    #   0.09932161 -0.02168292 0.003857599
    # 
    # Degrees of Freedom: 25 Total; 22 Residual
    # Residual Deviance: 4.025066 
    # 
    # 
    # > 
    # 




cleanEx(); nameEx("lm.mids");
### * lm.mids

flush(stderr()); flush(stdout())

### Name: lm.mids
### Title: Linear Regression on Multiply Imputed Data
### Aliases: lm.mids
### Keywords: misc

### ** Examples

data(nhanes)
imp <- mice(nhanes)     # do default multiple imputation on a numeric matrix
fit <- lm.mids(bmi~hyp+chl,data=imp)



cleanEx(); nameEx("md.pattern");
### * md.pattern

flush(stderr()); flush(stdout())

### Name: md.pattern
### Title: Missing Data Pattern
### Aliases: md.pattern
### Keywords: misc

### ** Examples

data(nhanes)
md.pattern(nhanes)
#     age hyp bmi chl    
#  13   1   1   1   1  0
#   1   1   1   0   1  1
#   3   1   1   1   0  1
#   1   1   0   0   1  2
#   7   1   0   0   0  3
#   0   8   9  10 27




cleanEx(); nameEx("mice");
### * mice

flush(stderr()); flush(stdout())

### Name: mice
### Title: Multivariate Imputation by Chained Equations
### Aliases: mice
### Keywords: misc

### ** Examples

data(nhanes)
imp <- mice(nhanes)     # do default multiple imputation on a numeric matrix
imp
imp$imputations$bmi     # and list the actual imputations 
complete(imp)       # show the first completed data matrix
lm.mids(chl~age+bmi+hyp, imp)   # repeated linear regression on imputed data

data(nhanes2)
mice(nhanes2,im=c("sample","pmm","logreg","norm")) # imputation on mixed data with a different method per column



cleanEx(); nameEx("mice.mids");
### * mice.mids

flush(stderr()); flush(stdout())

### Name: mice.mids
### Title: Multivariate Imputation by Chained Equations (Iteration Step)
### Aliases: mice.mids
### Keywords:  misc

### ** Examples

data(nhanes)
imp1 <- mice(nhanes,maxit=1)
imp2 <- mice.mids(imp1)

# yields the same result as
imp <- mice(nhanes,maxit=2)

# for example:
# 
# > imp$imp$bmi[1,]
#      1    2    3    4    5 
# 1 30.1 35.3 33.2 35.3 27.5
# > imp2$imp$bmi[1,]
#      1    2    3    4    5 
# 1 30.1 35.3 33.2 35.3 27.5
# 



cleanEx(); nameEx("pool");
### * pool

flush(stderr()); flush(stdout())

### Name: pool
### Title: Multiple Imputation Pooling
### Aliases: pool
### Keywords: misc

### ** Examples

data(nhanes)
imp <- mice(nhanes)
fit <- lm.mids(bmi~hyp+chl,data=imp)
pool(fit)
#  Call: pool(object = fit)
#  Pooled coefficients:
#   (Intercept)       hyp        chl 
#      21.29782 -1.751721 0.04085703
#
#  Fraction of information about the coefficients missing due to nonrespons
#  e: 
#   (Intercept)       hyp       chl 
#     0.1592247 0.1738868 0.3117452
#
#  > summary(pool(fit))
#           est         se          t       df     Pr(>|t|) 
#  (Intercept)  21.29781702 4.33668150  4.9110863 16.95890 0.0001329371
#      hyp  -1.75172102 2.30620984 -0.7595671 16.39701 0.4582953905
#      chl   0.04085703 0.02532914  1.6130442 11.50642 0.1338044664
#             lo 95      hi 95 missing       fmi 
#  (Intercept)  12.14652927 30.4491048      NA 0.1592247
#      hyp  -6.63106456  3.1276225       8 0.1738868
#    chl  -0.01459414  0.0963082      10 0.3117452 



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

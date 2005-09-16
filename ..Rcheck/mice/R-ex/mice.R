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




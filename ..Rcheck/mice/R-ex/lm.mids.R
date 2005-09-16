### Name: lm.mids
### Title: Linear Regression on Multiply Imputed Data
### Aliases: lm.mids
### Keywords: misc

### ** Examples

data(nhanes)
imp <- mice(nhanes)     # do default multiple imputation on a numeric matrix
fit <- lm.mids(bmi~hyp+chl,data=imp)




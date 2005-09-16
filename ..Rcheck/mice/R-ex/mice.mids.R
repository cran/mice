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




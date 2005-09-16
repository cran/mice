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




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




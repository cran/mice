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





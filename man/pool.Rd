\name{pool}
\alias{pool}
\title{Multiple Imputation Pooling}

\description{
  Pools the results of m repeated complete data analysis
}
\usage{
pool(object, method="smallsample")
}

\arguments{
  \item{object}{An object of class 'mira', produced by functions like lm.mids or glm.mids.}
  \item{method}{ A string describing the method to compute the degrees of freedom. 
    The default value is "smallsample", which specifies the is 
    Barnard-Rubin adjusted degrees of freedom (Barnard& Rubin, 1999) 
    for small samples. Specifying a different string 
    produces the conventional degrees of freedom as in Rubin (1987).}
}   
\value{
  An object of class 'mipo', which stands for 'multiple imputation pooled'.
}
\details{
  The function averages the estimates of the complete data model, 
  computes the total variance over the repeated analyses, and computes
  the relative increase in variance due to nonresponse and the fraction 
  of missing information. The function relies on the availability
  of
  \enumerate{
    \item the estimates of the model, typically present as 'coefficients' in 
    the fit object
    \item an appropriate estimate of the variance-covariance matrix of the 
    estimates per analyses.
  }
  R-Specific: The original use of Varcov has been removed to vcov (VR MASS).
}   

\references{
  Barnard, J. and Rubin, D.B. (1999). Small sample degrees of freedom with
  multiple imputation. Biometrika, 86, 948-955.
  
  Rubin, D.B. (1987). Multiple Imputation for Nonresponse in Surveys. 
  New York: John Wiley and Sons.
  
  Alzola, C.F. and Harrell, F.E. (1999). An introduction to S-Plus and the Hmisc 
  and Design Libraries. http://hesweb1.med.virginia.edu/biostat/s/index.html. 
}
\seealso{
  \code{\link{lm.mids}}, \code{\link{glm.mids}}, \code{\link{vcov}},
  \code{\link{print.mira}}, \code{\link{summary.mira}}
}
\examples{
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
}



\author{Stef van Buuren, Karin Oudshoorn, 2000} 

\keyword{misc}  

\name{glm.mids}
\alias{glm.mids}
\title{Generelized Linear Regression on Multiply Imputed Data}

\description{
  Performs repeated glm on a multiply imputed data set
}
\usage{
    glm.mids(formula, data, ...)
}

\arguments{
  \item{formula}{   a formula expression as for other regression models, of the form 
    response ~ predictors. See the documentation
    of \code{\link{lm}} and \code{\link{formula}} for details.}
  
  \item{data}{  An object of type \code{mids}, which stands for 'multiply imputed data set', typically
    created by function \code{mice()}.}
  
  \item{...}{ Additional parameters passed to \code{\link{glm}}.}
}
\value{
  An objects of class \code{mira}, which stands for 'multiply imputed repeated analysis'.
  This object contains m \code{glm.objects}, plus some descriptive information.
}

\details{
  see \code{\link{glm}}
}

\references{
  Van Buuren, S. & Oudshoorn, C.G.M. (2000). Multivariate Imputation by Chained Equations: 
  MICE V1.0 User's manual. Report PG/VGZ/00.038, TNO Prevention and Health, Leiden.
}
\seealso{
  \code{\link{glm}},  \code{\link{mids}}, \code{\link{mira}}
}
\examples{
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

}

\author{Stef van Buuren, Karin Oudshoorn, 2000} 

\keyword{misc} 

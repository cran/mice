\name{impute.polyreg}
\alias{impute.polyreg}
\title{Elementary Imputation Method: Polytomous Regression}
\usage{
impute.polyreg(y, ry, x)
}
\description{Imputes missing data in a categorical variable using polytomous regression
}
\arguments{
  \item{y}{ Incomplete data vector of length n}
  \item{ry}{    Vector of missing data pattern (FALSE=missing, TRUE=observed)}
  \item{x}{ Matrix (n x p) of complete covariates.}
}
\value{A vector of length nmis with imputations.
}
\details{
  Imputation for categorical response variables by the Bayesian 
  polytomous regression model. See J.P.L. Brand (1999), Chapter 4,
  Appendix B.
  
  The method consists of the following steps:
  \begin{enumerate}
  \item Fit categorical response as a multinomial model 
  \item Compute predicted categories
  \item Add appropriate noise to predictions.
  \end{enumerate}
  This algorithm uses the function multinom from the libraries nnet and MASS
  (Venables and Ripley). 
}
\references{
  Van Buuren, S. & Oudshoorn, C.G.M. (2000). Multivariate Imputation by Chained Equations: 
  MICE V1.0 User's manual. Report PG/VGZ/00.038, TNO Prevention and Health, Leiden.
  
  Brand, J.P.L. (1999). Development, Implementation and Evaluation of Multiple Imputation Strategies for the Statistical Analysis of Incomplete Data Sets. Ph.D. Thesis, TNO Prevention and Health/Erasmus University Rotterdam. ISBN 90-74479-08-1. 
  
}
\seealso{\code{\link{mice}}, \code{\link{multinom}}
}

\author{Stef van Buuren, Karin Oudshoorn, 2000}
    
\keyword{misc} 

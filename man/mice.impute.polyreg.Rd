\name{mice.impute.polyreg}
\alias{mice.impute.polyreg}
\title{Elementary Imputation Method: Polytomous Regression}
\usage{
mice.impute.polyreg(y, ry, x)
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
  \enumerate{
    \item Fit categorical response as a multinomial model 
    \item Compute predicted categories
    \item Add appropriate noise to predictions.
  }
  This algorithm uses the function \code{multinom()} from the libraries \code{nnet} 
  (Venables and Ripley). 
}
\references{
Van Buuren, S., Groothuis-Oudshoorn, C.G.M. (2000) 
\emph{Multivariate Imputation by Chained Equations:  MICE V1.0 User's manual.} 
Leiden: TNO Quality of Life.
\url{http://www.stefvanbuuren.nl/publications/MICE V1.0 Manual TNO00038 2000.pdf}
  
Brand, J.P.L. (1999) 
\emph{Development, implementation and evaluation of multiple imputation strategies for the statistical analysis of incomplete data sets.} 
Dissertation. Rotterdam: Erasmus University.
  
Venables, W.N. & Ripley, B.D. (1997). Modern applied statistics with S-Plus (2nd ed). Springer, Berlin.

}
\seealso{\code{\link{mice}}, \code{\link[nnet]{multinom}}
}

\author{Stef van Buuren, Karin Groohuis-Oudshoorn, 2000}
    
\keyword{misc} 

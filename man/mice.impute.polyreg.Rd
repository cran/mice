\name{mice.impute.polyreg}
\alias{mice.impute.polyreg}
\title{Imputation by Polytomous Regression}
\usage{
mice.impute.polyreg(y, ry, x)
}
\description{Imputes missing data in a categorical variable using polytomous regression
}
\arguments{
  \item{y}{ Incomplete data vector of length \code{n}}
  \item{ry}{    Vector of missing data pattern (\code{FALSE}=missing, \code{TRUE}=observed)}
  \item{x}{ Matrix (\code{n} x \code{p}) of complete covariates.}
}
\value{A vector of length \code{nmis} with imputations.
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
Van Buuren, S., Groothuis-Oudshoorn, K. (2009) 
MICE: Multivariate Imputation by Chained Equations in R. 
\emph{Journal of Statistical Software}, forthcoming.
\url{http://www.stefvanbuuren.nl/publications/MICE in R - Draft.pdf}
  
Brand, J.P.L. (1999) 
\emph{Development, implementation and evaluation of multiple imputation strategies for the statistical analysis of incomplete data sets.} 
Dissertation. Rotterdam: Erasmus University.
  
Venables, W.N. & Ripley, B.D. (1997). Modern applied statistics with S-Plus (2nd ed). Springer, Berlin.

}
\seealso{\code{\link{mice}}, \code{\link[nnet]{multinom}}
}

\author{Stef van Buuren, Karin Groohuis-Oudshoorn, 2000}
    
\keyword{datagen} 

\name{mice.impute.norm.nob}
\alias{mice.impute.norm.nob}
\title{Imputation by Linear Regression (non Bayesian)}
\usage{
    mice.impute.norm.nob(y, ry, x)
}
\description{Imputes univariate missing data using linear regression analysis (non Bayesian version)
}
\arguments{
  \item{y}{Incomplete data vector of length \code{n}}
  \item{ry}{Vector of missing data pattern (\code{FALSE}=missing, \code{TRUE}=observed)}
  \item{x}{Matrix (\code{n} x \code{p}) of complete covariates.}
}
\value{A vector of length \code{nmis} with imputations.
}
\details{This creates imputation using the spread around the fitted 
  linear regression line of \code{y} given \code{x}, as fitted on the observed data.
  
}
\section{Warning}{
  The function does not incorporate the variability of the regression
  weights, so it is not 'proper' in the sense of Rubin. For small samples, 
  variability of the imputed data is therefore underestimated. 
}
\references{
Van Buuren, S., Groothuis-Oudshoorn, K. (2009) 
MICE: Multivariate Imputation by Chained Equations in R. 
\emph{Journal of Statistical Software}, forthcoming.
\url{http://www.stefvanbuuren.nl/publications/MICE in R - Draft.pdf}
  
  Brand, J.P.L. (1999). Development, Implementation and Evaluation of
  Multiple Imputation Strategies for the Statistical Analysis of
  Incomplete Data Sets. Ph.D. Thesis, TNO Prevention
  and Health/Erasmus University Rotterdam.
  
}
\note{  This function is provided mainly to allow comparison between proper
  and improper norm methods. Also, it may be useful to impute large data containing 
  many rows.
}
\seealso{\code{\link{mice}},  \code{\link{mice.impute.norm}}
}


\author{Stef van Buuren, Karin Groothuis-Oudshoorn, 2000}

\keyword{datagen} 
